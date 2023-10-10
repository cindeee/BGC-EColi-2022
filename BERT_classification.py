# %% [markdown]
# <h2 align=center> Fine-Tune BERT for Text Classification with TensorFlow</h2>

# %% [markdown]
# <div align="center">
#     <img width="512px" src='https://drive.google.com/uc?id=1fnJTeJs5HUpz7nix-F9E6EZdgUflqyEu' />
#     <p style="text-align: center;color:gray">Figure 1: BERT Classification Model</p>
# </div>

# %% [markdown]
# aim: fine-tune a BERT model for text classification using TensorFlow and TF-Hub.
# 
# > Indented block
# 
# 

# %% [markdown]
# The pretrained BERT model :  [here](https://tfhub.dev/tensorflow/bert_en_uncased_L-12_H-768_A-12/2) on [TensorFlow Hub](https://tfhub.dev/).

# %% [markdown]
# ### Contents

# %% [markdown]
# This project/notebook consists of several Tasks.
# 
# - **[Task 1]()**: Introduction to the Project.
# - **[Task 2]()**: Setup your TensorFlow and Colab Runtime
# - **[Task 3]()**: Download and Import the Quora Insincere Questions Dataset
# - **[Task 4]()**: Create tf.data.Datasets for Training and Evaluation
# - **[Task 5]()**: Download a Pre-trained BERT Model from TensorFlow Hub
# - **[Task 6]()**: Tokenize and Preprocess Text for BERT
# - **[Task 7]()**: Wrap a Python Function into a TensorFlow op for Eager Execution
# - **[Task 8]()**: Create a TensorFlow Input Pipeline with `tf.data`
# - **[Task 9]()**: Add a Classification Head to the BERT `hub.KerasLayer`
# - **[Task 10]()**: Fine-Tune BERT for Text Classification
# - **[Task 11]()**: Evaluate the BERT Text Classification Model

# %% [markdown]
# ## Task 2: Setup your TensorFlow and Colab Runtime.

# %%


# %%
!nvidia- # util for checking GPU: Tesla T4

# %% [markdown]
# ### Install TensorFlow and TensorFlow Model Garden

# %%
import tensorflow as tf
print(tf.version.VERSION)
# !pip install -q tensorflow==2.3.0 # at least 

# %%
!git clone --depth 1 -b v2.3.0 https://github.com/tensorflow/models.git # to mnodels directory 

# %%
# install requirements to use tensorflow/models repository
!pip install -Uqr models/official/requirements.txt
# restart the runtime afterwards

# %% [markdown]
# ## Task 3: Download and Import the Quora Insincere Questions Dataset

# %%
import numpy as np
import tensorflow as tf
import tensorflow_hub as hub
import sys
sys.path.append('models')
from official.nlp.data import classifier_data_lib
from official.nlp.bert import tokenization
from official.nlp import optimization

print("TF Version: ", tf.__version__)
print("Eager mode: ", tf.executing_eagerly())
print("Hub version: ", hub.__version__)
print("GPU is", "available" if tf.config.experimental.list_physical_devices("GPU") else "NOT AVAILABLE")

# %% [markdown]
# A downloadable copy of the [Quora Insincere Questions Classification data](https://www.kaggle.com/c/quora-insincere-questions-classification/data) can be found [https://archive.org/download/fine-tune-bert-tensorflow-train.csv/train.csv.zip](https://archive.org/download/fine-tune-bert-tensorflow-train.csv/train.csv.zip). Decompress and read the data into a pandas DataFrame.

# %%
import numpy as np
import pandas as pd
from sklearn.model_selection import train_test_split

# dataset
df = pd.read_csv('https://archive.org/download/fine-tune-bert-tensorflow-train.csv/train.csv.zip', compression = 'zip', low_memory = False)

df.shape

# %%
# last 20 rows
df.tail(20) # quid, question_text, target

# %%
df.target.plot(kind = 'hist', title = 'Target distribution'); 
# highly imbalenced dataset : majority are sincere
# solve by stratified sampling

# %% [markdown]
# ## Task 4: Create tf.data.Datasets for Training and Evaluation

# %%
# training, validation set 
train_df, remaining = train_test_split(df, random_state = 42, 
                                       train_size = 0.0075, # time limit on project
                                       stratify = df.target.values)
valid_df, _ = train_test_split(remaining, random_state = 42, train_size = 0.00075, stratify = remaining.target.values)
train_df.shape, valid_df.shape

# %%
# return python iterable dataset obj
with tf.device('/cpu:0'): 
  train_data = tf.data.Dataset.from_tensor_slices((train_df['question_text'].values, train_df['target'].values))
  valid_data = tf.data.Dataset.from_tensor_slices((valid_df.question_text.values, valid_df.target.values))

  for text, label in train_data.take(1): # look at 1st entry. demo: can use for loop. 
    print(text)
    print(label)

# %% [markdown]
# ## Task 5: Download a Pre-trained BERT Model from TensorFlow Hub
# 
# https://tfhub.dev/tensorflow/bert_en_uncased_L-12_H-768_A-12/2

# %%
"""
Each line of the dataset is composed of the review text and its label
- Data preprocessing consists of transforming text to BERT input features:
input_word_ids, input_mask, segment_ids
- In the process, tokenizing the text is done with the provided BERT model tokenizer
"""

label_list = [0,1] # Label categories. 0 = sincere. 1 = insincere
max_seq_length = 128  # maximum length of (token) input sequences
train_batch_size = 32 


# Get BERT layer and tokenizer:
# More details here: https://tfhub.dev/tensorflow/bert_en_uncased_L-12_H-768_A-12/2
bert_layer = hub.KerasLayer('https://tfhub.dev/tensorflow/bert_en_uncased_L-12_H-768_A-12/2', trainable = True) # default = F

vocab_file = bert_layer.resolved_object.vocab_file.asset_path.numpy()
do_lower_case = bert_layer.resolved_object.do_lower_case.numpy()
tokenizer = tokenization.FullTokenizer(vocab_file, do_lower_case)


# %%
a = tokenizer.wordpiece_tokenizer.tokenize('hi, how are you doing?')
print(a)
# convert token to id 
print(tokenizer.convert_tokens_to_ids(a))

# %% [markdown]
# ## Task 6: Tokenize and Preprocess Text for BERT

# %% [markdown]
# <div align="center">
#     <img width="512px" src='https://drive.google.com/uc?id=1-SpKFELnEvBMBqO7h3iypo8q9uUUo96P' />
#     <p style="text-align: center;color:gray">Figure 2: BERT Tokenizer</p>
# </div>

# %% [markdown]
# We'll need to transform our data into a format BERT understands. This involves two steps. First, we create InputExamples using `classifier_data_lib`'s constructor `InputExample` provided in the BERT library.

# %%
# This provides a function to convert row to input features and label
# 0, 1, 2 : prepended to start seq, appended to end of seq

def to_feature(text, label, label_list = label_list, max_seq_length = max_seq_length, tokenizer = tokenizer):
  example = classifier_data_lib.InputExample(guid = None,
                                             text_a = text.numpy(),
                                             text_b = None, 
                                             label = label.numpy())
  feature = classifier_data_lib.convert_single_example(0, example, label_list, max_seq_length, tokenizer)

  return (feature.input_ids, feature.input_mask, feature.segment_ids, feature.label_id)

# %% [markdown]
# You want to use [`Dataset.map`](https://www.tensorflow.org/api_docs/python/tf/data/Dataset#map) to apply this function to each element of the dataset. [`Dataset.map`](https://www.tensorflow.org/api_docs/python/tf/data/Dataset#map) runs in graph mode.
# 
# - Graph tensors do not have a value.
# - In graph mode you can only use TensorFlow Ops and functions.
# 
# So you can't `.map` this function directly: You need to wrap it in a [`tf.py_function`](https://www.tensorflow.org/api_docs/python/tf/py_function). The [`tf.py_function`](https://www.tensorflow.org/api_docs/python/tf/py_function) will pass regular tensors (with a value and a `.numpy()` method to access it), to the wrapped python function.

# %% [markdown]
# ## Task 7: Wrap a Python Function into a TensorFlow op for Eager Execution

# %%
def to_feature_map(text, label):
  input_ids, input_mask, segment_ids, label_id = tf.py_function(to_feature, inp = [text, label], 
                                                                Tout = [tf.int32, tf.int32, tf.int32, tf.int32])
  input_ids.set_shape([max_seq_length])
  input_mask.set_shape([max_seq_length])
  segment_ids.set_shape([max_seq_length])
  label_id.set_shape([])

  x = {
      'input_word_ids': input_ids,
      'input_mask' :input_mask,
      'input_type_ids' : segment_ids
  }
  return (x, label_id)

# %% [markdown]
# ## Task 8: Create a TensorFlow Input Pipeline with `tf.data`

# %%
with tf.device('/cpu:0'):
  # train
  train_data = (train_data.map(to_feature_map,
                               num_parallel_calls = tf.data.experimental.AUTOTUNE) # optimise the best parallel calls. can be customised
  .shuffle(1000) # only on training 
  .batch(32, drop_remainder= True) #remaining barth 
  .prefetch(tf.data.experimental.AUTOTUNE))

  # valid
  valid_data = (valid_data.map(to_feature_map,
                               num_parallel_calls = tf.data.experimental.AUTOTUNE)
  .shuffle(1000)
  .batch(32, drop_remainder= True) 
  .prefetch(tf.data.experimental.AUTOTUNE))
  

# %% [markdown]
# The resulting `tf.data.Datasets` return `(features, labels)` pairs, as expected by [`keras.Model.fit`](https://www.tensorflow.org/api_docs/python/tf/keras/Model#fit):

# %%
# train data spec
train_data.element_spec

# %%
# valid data spec
valid_data.element_spec

# %% [markdown]
# ## Task 9: Add a Classification Head to the BERT Layer

# %% [markdown]
# <div align="center">
#     <img width="512px" src='https://drive.google.com/uc?id=1fnJTeJs5HUpz7nix-F9E6EZdgUflqyEu' />
#     <p style="text-align: center;color:gray">Figure 3: BERT Layer</p>
# </div>

# %%
# Building the model
# pooled: entire embedding of input sequence 
def create_model():
  input_word_ids = tf.keras.layers.Input(shape=(max_seq_length), dtype=tf.int32, name = 'input_word_ids')
  input_mask = tf.keras.layers.Input(shape=(max_seq_length), dtype=tf.int32, name = 'input_mask')
  input_type_ids = tf.keras.layers.Input(shape=(max_seq_length), dtype=tf.int32, name = 'input_type_ids')
  
  pooled_output, sequence_output = bert_layer([input_word_ids, input_mask, input_type_ids])
  #preprocessor = hub.KerasLayer("https://tfhub.dev/tensorflow/bert_en_uncased_preprocess/3")
  #encoder_inputs = preprocessor(input_q)
  #encoder = hub.KerasLayer("https://tfhub.dev/tensorflow/bert_en_uncased_L-12_H-768_A-12/4", trainable =True)
  #outputs = encoder(encoder_inputs)
  #pooled_output = outputs["pooled_output"]     
  #sequence_output = outputs["sequence_output"]  

  drop = tf.keras.layers.Dropout(0.4)(pooled_output) # drop regularisation
  output = tf.keras.layers.Dense(1, activation = 'sigmoid', name = 'output')(drop) # 2-class. higher than threshold -> +ve
  model = tf.keras.Model(
      inputs = {
          'input_word_ids': input_word_ids,
          'input_mask': input_mask,
          'input_type_ids': input_type_ids
      },
      outputs = output)
  return model

# %% [markdown]
# ## Task 10: Fine-Tune BERT for Text Classification

# %%
model = create_model()
model.compile(optimizer = tf.keras.optimizers.Adam(learning_rate = 2e-5),
              loss = tf.keras.losses.BinaryCrossentropy(),
              metrics = [tf.keras.metrics.BinaryAccuracy()])
model.summary()

# %%
tf.keras.utils.plot_model(model = model, show_shapes = True, dpi = 76) # entire transformer model to single input layer

# %%
# Train model
epochs = 4
history = model.fit(train_data, 
                    validation_data = valid_data, 
                    epochs = epochs,
                    verbose = 1)    # generate ouput n

# %% [markdown]
# ## Task 11: Evaluate the BERT Text Classification Model

# %%
import matplotlib.pyplot as plt

def plot_graphs(history, metric):
  plt.plot(history.history[metric])
  plt.plot(history.history['val_'+metric], '')
  plt.xlabel("Epochs")
  plt.ylabel(metric)
  plt.legend([metric, 'val_'+metric])
  plt.show()

# %%
plot_graphs(history, 'loss')

# %%
plot_graphs(history, 'binary')

# %%
sample_example = ['sentence 1']
test_data = tf.data.Dataset.from_tensor_slices((sample_example,[0]*len(sample_example)))
test_data = (test_data.map(to_feature_map).batch(1))
preds = model.predict(test_data)
threshold = 0.8 #between 0, 1
['Insincere' if pred>= threshold else 'Sincere' for pred in preds]





