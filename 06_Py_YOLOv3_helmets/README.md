# YOLO Object detection and couple thoughts on safety issue

## What is it?

This is a project developed as a final project at the Data Science bootcamp organised by Sages.

The basic aim is to prepare a prototype of a model based on YOLOv3 architecture using transfer learning and train it on an existing base of the images of people with and without helmet.

## Dataset

The dataset was obtained from [kaggle task](https://www.kaggle.com/andrewmvd/hard-hat-detection) described as *Improve workplace safety by detecting people and hard hats on 5k images with bbox annotations*, however the origin source of date is server [MakeML](https://makeml.app/datasets/hard-hat-workers) that allow to create object detection and segmentation ML models without code (written by user).

This dataset contains 5000 images (png file 416x416px) and corresponding to them 5000 xml file with bounding boxes and annotations in the PASCAL VOC format for 3 classes: *helmet*, *person*, *head*.

## Models&tools

Models were build with help of [Tensorflow 2 Object Detection API](https://github.com/tensorflow/models/tree/master/research/object_detection) and trained on Google Cloud AI Platform.

The Base code of SSD model comes from a collection of detection models pre-trained on the COCO 2017 dataset that can be found in [TensorFlow 2 Detection Model Zoo](https://github.com/tensorflow/models/blob/master/research/object_detection/g3doc/tf2_detection_zoo.md). The backbone (the feature extractor) is 152-layer ResNets (ResNet 152 v1 FPN denoted as RetinaNet152) trained from the Common Objects in Context (COCO) image dataset. 

The base code of YOLOv3 model comes from [YunYang github](https://github.com/YunYang1994/TensorFlow2.0-Examples/tree/master/4-Object_Detection/YOLOV3) with [original YOLOv3 weights](https://pjreddie.com/media/files/yolov3.weights) also coming from training on The PASCAL VOC dataset. The backbone (the feature extractor) is Darknet53

## Result

Project description and results can be found [here](http://j-smola.github.io/repository/Project_JS.html).
