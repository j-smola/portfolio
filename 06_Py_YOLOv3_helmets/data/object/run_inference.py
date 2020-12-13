import cv2
import os
import shutil
import numpy as np
import tensorflow as tf
from data.object.saved_model.config import cfg
import data.object.saved_model.utils as utils
import matplotlib.pyplot as plt
from PIL import Image, ImageDraw, ImageFont


def showIMG_MODEL(img_name, path_to_image = './data/test', confidence_score = 0.7, iou = 0.5):
    """
    Plot image with bounding box predicted by frozen model - YOLOv3_2
    
    _ARGS
    img_name:         str     name of image
    path_to_image:    str     source dir of imgage (path)
    confidence_score: float   confidence of predicted class
    iou:              float   evaluation metric predicted bbox vs. groundtruth
    
    _OUTPUTS
    image with bounding boxes+labels with class name and confidence score
    """
    # model's parameters
    INPUT_SIZE = cfg.TEST.INPUT_SIZE
    SCORE_THRESHOLD = confidence_score
    IOU_THRESHOLD = iou
    
    # model
    model = tf.keras.models.load_model('./data/object/saved_model/YOLOv3_model', compile=False)
    
    # prepare img to model 
    image = cv2.imread(os.path.join(path_to_image, img_name))
    image = cv2.cvtColor(image, cv2.COLOR_BGR2RGB)
    image_size = image.shape[:2]
    image_data = utils.image_preporcess(np.copy(image), [INPUT_SIZE, INPUT_SIZE])
    image_data = image_data[np.newaxis, ...].astype(np.float32) # (1, width, height, 3)
    
    # prediction of bboxes
    pred_bbox = model.predict(image_data)
    pred_bbox = [tf.reshape(x, (-1, tf.shape(x)[-1])) for x in pred_bbox]
    pred_bbox = tf.concat(pred_bbox, axis=0)
    bboxes = utils.postprocess_boxes(pred_bbox, image_size, INPUT_SIZE, SCORE_THRESHOLD)
    bboxes = utils.nms(bboxes, IOU_THRESHOLD, method='nms')
    
    # dict with colors for classes/names of model
    dict_col = {'helmet':'deeppink', 'head':'gold', 'person':'limegreen'}
    dict_class = {0:'head', 1:'helmet', 2:'person'}
    
    # draw img with predicted bboxes
    img = Image.open(os.path.join(path_to_image, img_name))
    ratio = img.size[1]/416
    draw = ImageDraw.Draw(img)
    font = ImageFont.truetype(font="./data/object/arial.ttf", size=(img.size[0] + img.size[1]) // 75)
    ratio = img.size[1] / 416
    
    for bbox in bboxes:
        *ls_val, prob0, lbl0 = bbox
        xmin, ymin, xmax, ymax = [int(v) for v in ls_val]
        prob = '%.0f' % (float(prob0)*100) + "%"
        lbl = dict_class[lbl0]
        lbl_text = ' ' + lbl + ' ' + prob
 
        draw.rectangle([xmin, ymin, xmax, ymax], outline=dict_col.get(lbl), width=int(ratio))
        text_size = draw.textsize(lbl_text, font=font)

        if ymin < text_size[1]:
            draw.rectangle(
                [xmin, ymax + text_size[1], xmin + text_size[0], ymax],
                fill=dict_col.get(lbl))
            draw.text((xmin, ymax), lbl_text, fill='black',
                      font=font)
        elif xmin + text_size[0] > img.size[0]:
            draw.rectangle(
                [xmax - text_size[0], ymin - text_size[1], xmax, ymin],
                fill=dict_col.get(lbl))
            draw.text((xmax - text_size[0], ymin - text_size[1]), lbl_text, fill='black',
                      font=font)
        else:
            draw.rectangle(
                [xmin, ymin - text_size[1], xmin + text_size[0], ymin],
                fill=dict_col.get(lbl))
            draw.text((xmin, ymin - text_size[1]), lbl_text, fill='black',
                      font=font)

    new_width, new_height = int(img.size[0]/ratio), 416
    final_im = img.resize((new_width, new_height))
    display(final_im)   
