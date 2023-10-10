# %% [markdown]
# # Task 1 : Set up colab gpu runtime environment

# %%
!pip install segmentation-models-pytorch
!pip install -U git+https://github.com/albumentations-team/albumentations
!pip install --upgrade opencv-contrib-python

# %% [markdown]
# # Download Dataset
# 
# original author of the dataset :
# https://github.com/VikramShenoy97/Human-Segmentation-Dataset
# 

# %%
!git clone https://github.com/parth1620/Human-Segmentation-Dataset-master.git

# %% [markdown]

# %% [markdown]
# # Some Common Imports

# %%
import sys
sys.path.append('/content/Human-Segmentation-Dataset-master')

# %%
import torch 
import cv2

import numpy as np 
import pandas as pd
import matplotlib.pyplot as plt 

from sklearn.model_selection import train_test_split
from tqdm import tqdm

import helper

# %% [markdown]
# # Task : 2 Setup Configurations

# %%
CSV_FILE = '/content/Human-Segmentation-Dataset-master/train.csv'
DATA_DIR = '/content/'

DEVICE = 'cuda'

EPOCHS = 25
LR = 0.003
IMG_SIZE = 320 #GIVEN
BATCH_SIZE = 16

ENCODER = 'timm-efficientnet-b0'
WEIGHTS = 'imagenet'

# %%
df = pd.read_csv(CSV_FILE)
df.head() #mask, images

# %%
row = df.iloc[4]   # eg4
image_path = row.images
mask_path = row.masks

image = cv2.imread(image_path) # load image
image = cv2.cvtColor(image, cv2.COLOR_BGR2RGB) # convert color spaces, 150+ options

mask = cv2.imread(mask_path, cv2.IMREAD_GRAYSCALE) / 255.0

# %%
f, (ax1, ax2) = plt.subplots(1, 2, figsize=(10,5))
        
ax1.set_title('IMAGE')
ax1.imshow(image)

ax2.set_title('GROUND TRUTH')
ax2.imshow(mask,cmap = 'gray')

# %%
# split 
train_df, valid_df = train_test_split(df, test_size = 0.2, random_state = 42)

# %% [markdown]
# # Task 3 : Augmentation Functions

# %% [markdown]
# albumentation documentation : https://albumentations.ai/docs/
# 
# we apply augmentation on image, not labels.
# training, ground truth dataset with masks go through the same augmentation processing. 

# %%
import albumentations as A

# %%
def get_train_augs():
  return A.Compose([
      A.Resize(IMG_SIZE, IMG_SIZE),
      A.HorizontalFlip(p = 0.5), # augmentations
      A.VerticalFlip(p = 0.5)
  ])

def get_valid_augs():
  return A.Compose([
      A.Resize(IMG_SIZE, IMG_SIZE),
  ])

# %% [markdown]
# # Task 4 : Create Custom Dataset 

# %%
from torch.utils.data import Dataset

# %%
class SegmentationDataset(Dataset):

  def __init__(self, df, augmentations):  
    self.df = df
    self.augmentations = augmentations

  def __len__(self):
    return len(self.df)

  def __getitem__(self, idx): 
    row = self.df.iloc[idx]

    image_path = row.images
    mask_path = row.masks

    image = cv2.imread(image_path)
    image = cv2.cvtColor(image, cv2.COLOR_BGR2RGB)

    mask = cv2.imread(mask_path, cv2.IMREAD_GRAYSCALE) 
    mask= np.expand_dims(mask, axis = -1)  # (h, w, c)

    if self.augmentations: 
      data = self.augmentations(image = image, mask = mask ) # return dict format: 
      image = data['image']
      mask = data['mask']

    # (h, w, c) -> (c, h, w) , pytorch format

    image = np.transpose(image, (2, 0, 1)).astype(np.float32)
    mask = np.transpose(mask, (2, 0, 1)).astype(np.float32)

    image = torch.Tensor(image) / 255.0   # scale to be 0-1
    mask = torch.round(torch.Tensor(mask) / 255.0)
    
    return image, mask

# %%
trainset = SegmentationDataset(train_df, get_train_augs())
validset = SegmentationDataset(valid_df, get_valid_augs())

# %%
print(f"Size of Trainset : {len(trainset)}")
print(f"Size of Validset : {len(validset)}")

# %%
idx = 38 # eg

image, mask = trainset[idx]
helper.show_image(image, mask)

# %% [markdown]
# # Task 5 : Load dataset into batches

# %%
from torch.utils.data import DataLoader

# %%
trainloader = DataLoader(trainset, batch_size = BATCH_SIZE, shuffle = True)
validloader = DataLoader(validset, batch_size = BATCH_SIZE)

# %%
print(f'total no. of batches in trainloader : {len(trainloader)}')
print(f'total no. of batches in validloader : {len(validloader)}')

# %%
for image, mask in trainloader: 
  print(f'One batch image shape : {image.shape}')
  print(f'One batch mask shape : {mask.shape}')
  break; # 1 batch for demo


# %% [markdown]
# # Task 6 : Create Segmentation Model

# %% [markdown]
# segmentation_models_pytorch documentation : https://smp.readthedocs.io/en/latest/

# %%
from torch import nn
import segmentation_models_pytorch as smp
from segmentation_models_pytorch.losses import DiceLoss

# %%
class SegmentationModel(nn.Module):

  def __init__(self):
    super(SegmentationModel, self).__init__()

    self.arc = smp.Unet(
        encoder_name = ENCODER,
        encoder_weights = WEIGHTS,
        in_channels = 3, 
        classes = 1,
        activation = None
    )

  def forward(self, images, masks = None): 
    
    logits = self.arc(images)

    if masks != None:
      loss1 = DiceLoss(mode = 'binary')(logits,masks)
      loss2 = nn.BCEWithLogitsLoss()(logits, masks)
      return logits, loss1 + loss2

    return logits

# %%
model = SegmentationModel()
model.to(DEVICE);

# %% [markdown]
# # Task 7 : Create Train and Validation Function 

# %%
def train_fn(data_loader, model, optimizer): 

  model.train()
  total_loss = 0.0

  for images, masks in tqdm(data_loader):
    images = images.to(DEVICE)
    masks = masks.to(DEVICE)

    optimizer.zero_grad()
    logits, loss = model(images, masks)
    loss.backward()
    optimizer.step()

    total_loss += loss.item()

  return total_loss / len(data_loader)

# %%
def eval_fn(data_loader, model): 

  model.eval() #separate, so no dropout
  total_loss = 0.0

  with torch.no_grad():
    for images, masks in tqdm(data_loader):
      images = images.to(DEVICE)
      masks = masks.to(DEVICE)

      logits, loss = model(images, masks)

      total_loss += loss.item()

  return total_loss / len(data_loader)

# %% [markdown]
# # Task 8 : Train Model

# %%
optimizer = torch.optim.Adam(model.parameters(), lr = LR)

# %%
best_valid_loss = np.Inf

for i in range(EPOCHS):
  train_loss = train_fn(trainloader, model, optimizer)
  valid_loss = eval_fn(validloader, model)

  if valid_loss < best_valid_loss:
    torch.save(model.state_dict(), 'best_model.pt')
    print('SAVED-MODEL')
    best_valid_loss = valid_loss

  print(f'epoch : {i+1} Train_loss : {train_loss} Valid_loss : {valid_loss}')

# %% [markdown]
# # Task 9 : Inference

# %%
idx = 21 # eg

model.load_state_dict(torch.load('/content/best_model.pt'))

image, mask = validset[idx]

logits_mask = model(image.to(DEVICE).unsqueeze(0)) # (c,h,w) -> (1, c, h, w)

pred_mask = torch.sigmoid(logits_mask)
pred_mask = (pred_mask > 0.5) * 1.0

# %%
helper.show_image(image, mask, pred_mask.detach().cpu().squeeze(0) )


