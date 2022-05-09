# License plate detection 
This project is used to illustrate my solo practicum project in Master's degree. The original project used the data from an insurance company. Due to non disclosure agreement, only the algorithms will be displayed here just for demonstration purposes. 

## Objectives
To train a license plate text retrieval model with image data as input

## Assumption
Since self-labelled data is used, this project is based on the assumption that license plate number on the training sample can be correctly identified by me.

## Method Used
- Haar Cascade Classifier
- Morphological transformation
- Object Character Recognition 

## Technologies
- Python
- OpenCV
- Tesseract

## Procedures
1. Using pretrained Haar Cascade Classifier for license plate detection
2. Performing morphological transformation (eg. resizing, grayscaling, Gaussian Blur, Canny edges detection, masking and cropping, etc.)  to prepare the images
3. Optical character recognition (OCR) with Tesseract
4. Accuracy calculation