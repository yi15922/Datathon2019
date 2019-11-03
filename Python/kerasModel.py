import tensorflow as tf
from tensorflow import keras
from tensorflow.keras.models import load_model
import h5py
import numpy as np
import matplotlib.pyplot as plt
import os
from keras.layers import Activation
from keras import backend as K
from keras.utils.generic_utils import get_custom_objects
import cv2
import PIL
from PIL import Image
import pandas as pd

print(os.getcwd())
training = pd.read_csv("../../data_python/training.csv")

training.head()