import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.patches as patches
import os
from PIL import Image, ImageDraw, ImageFont

def showIMG(no_image, img_path, tab_bndbox):
    """   
    Plot image with bounding box from annotation file
    
    _ARGS
    no_image:     int           the number of images
    images_path:  str           source of xml file (path)
    tab_bndbox:   pd.DataFrame  table with inforamtion about bounding box for image (output od parseXML)
    
    _OUTPUTS
    images     	        images with bounding box
    """
    
    images_name = 'hard_hat_workers' + str(no_image) + '.png'
    print(f'{no_image}: {images_name}')
    im = np.array(Image.open(os.path.join(img_path, images_name)), dtype=np.uint8)
    
    # create figure and axes
    fig, ax = plt.subplots(1, 2, figsize=(15,10))

    # display the image
    ax[0].imshow(im)
    ax[1].imshow(im)

    # create a rectangle patch
    dict_col = {'helmet':'deeppink', 'head':'gold', 'person':'limegreen'}
    
    for idx in tab_bndbox[tab_bndbox['no'] == int(no_image)].itertuples():
        lbl = idx[3]
        xmin, ymin, xmax, ymax = idx[4:]
     
        rect = patches.Rectangle((xmin, ymin), xmax-xmin, ymax-ymin,
                                 linewidth=2, edgecolor=dict_col.get(lbl), facecolor='none')
        ax[1].add_patch(rect)
#         ax[1].text(xmin, ymin, lbl, color=dict_col.get(lbl))

    plt.show()
    
    
def showRESULT(ls_result, model_type):
    """   
    Plot image with bounding box from annotation file
    
    _ARGS
    ls_result:   object   model result with data for classes (mPA, Precision, Recall, TP vs.FP)
    model_type:  str	   part of dahboard title - type of model    
    
    _OUTPUTS
    dashboard
    AP for classes + mAP   ||  ground_truth + TP vs.FP
    Precision-Recall Curve for classes
    """
    # object length
    LEN = len(ls_result)
    # number of class
    NO_C = LEN-3

    # mean average precision for model
    mAP = ls_result[LEN-3]['mAP_all_classes']
    mAP_val = '%.2f' % (ls_result[LEN-3]['mAP_all_classes']*100) + "%"

    # data
    tab_ap = []
    for i in range(NO_C):
        ls_x = {k: ls_result[i].get(k) for k in {'class', 'AP'}}
        tab_ap.append(ls_x)
    tab_ap = pd.DataFrame(tab_ap)
    tab_ap = tab_ap.sort_values(by='class',ascending=False).reset_index(drop=True)

    tab_gt = pd.DataFrame(ls_result[LEN-2]['ground_truth'])
    tab_pd = pd.DataFrame(ls_result[LEN-1]['pred_num'])
    tab_op = pd.merge(tab_gt, tab_pd, on='class', how='outer').fillna(int(0))
    tab_op = tab_op.sort_values(by='class',ascending=False).reset_index(drop=True)
    data_types_dict = {'class':str, 'gt':int, 'n_pred':int, 'tp':int, 'fp':int}
    tab_op = tab_op.astype(data_types_dict)
    
    ### 0 ###
    fig, ax = plt.subplots(1, 1, figsize=(15,0.01))
    ax.set_title('The result of evaluation for '+ model_type, fontsize=14)                         
    ax.axis('off')
    
    ### 1 ###
    fig, ax = plt.subplots(1, 2, figsize=(15,4))
    # Plot of AP for Classes
    ax[0].barh(y=tab_ap['class'], width=tab_ap['AP'], height=0.6, color='navy') 
    ax[0].axvline(x=mAP, color='darkorange')

    for i in range(len(tab_ap['class'])):
        AP_val_i = '%.2f' % (tab_ap['AP'][i]*100) + "%"
        ax[0].annotate(s=AP_val_i, xy = (tab_ap['AP'][i]+0.01, i), ha='left', va='center', color='navy', fontsize=14) 

    ax[0].set_xlabel('mAP')
    ax[0].set_title(f'mAP value for classes (mAP={mAP_val})')
    ax[0].set_xlim([0.0,1.1])
    x_value=['{:,.0f}'.format(x*100) + '%' for x in ax[0].get_xticks()]
    ax[0].set_xticklabels(x_value)

    # Plot of Ground Truth and TP vs. FP
    dict_col = ['purple', 'darkorange', 'darkgreen']
    ax[1].barh(y=tab_op['class'], width=tab_op['gt'], height=-0.4, align='edge', color=dict_col[0], label = 'Ground Truth') 
    ax[1].barh(y=tab_op['class'], width=tab_op['tp'], height=0.4, align='edge', color=dict_col[2], label='True Prediction')
    ax[1].barh(y=tab_op['class'], width=tab_op['fp'], left=tab_op['tp'], height=0.4, align='edge', color=dict_col[1], label = 'False Prediction')

    for i in range(len(tab_op['class'])):
        tp_col='white'
        fp_col='white'

        if tab_op['tp'][i] < 300:
            fp_x = 300
            tp_col = dict_col[2]
            fp_col = dict_col[1]
        elif tab_op['fp'][i] < 300:
            fp_x = tab_op['tp'][i]+tab_op['fp'][i]+50
            fp_col = dict_col[1]
        else:
            fp_x = tab_op['tp'][i]+100
        if tab_op['gt'][i] < 300:
            gt_col = dict_col[0]
        else: 
            gt_col='white'

        ax[1].annotate(s=tab_op['tp'][i], xy = (100, (i +.2)), ha='left', va='center', color=tp_col, fontsize=12) 
        ax[1].annotate(s=tab_op['fp'][i], xy = (fp_x, (i +.2)), ha='left', va='center', color=fp_col, fontsize=12) 
        ax[1].annotate(s=tab_op['gt'][i], xy = (100, (i -.2)), ha='left', va='center', color=gt_col, fontsize=12)   

    ax[1].set_xlabel('Number of objects per class')
    ax[1].set_title('Predicted Object')
    ax[1].legend()#loc='center left', bbox_to_anchor=(1, 0.5))

    ### 2 ###
    fig, ax = plt.subplots(1, NO_C, figsize=(15,2.5))
    # Plot of Precision-Recall Curve
    for i in range(NO_C):
        class_lbl = ls_result[i]['class']
        AP_val = '%.2f' % (ls_result[i]['AP']*100) + "%"
        tab_pr = pd.DataFrame({'Prec':[float(p) for p in ls_result[i]['Prec']],
                              'Recall':[float(r) for r in ls_result[i]['Recall']]})
        ax[i].plot(tab_pr['Recall'], tab_pr['Prec'], '-o', color='navy')


        ax[i].set_xlabel('Recall')
        ax[i].set_ylabel('Precision')
        ax[i].set_title(f'Precision-Recall Curve \n for class {class_lbl} (AP={AP_val})')
        mrec = tab_pr['Recall'].tolist()
        mprec = tab_pr['Prec'].tolist()
        area_under_curve_x = mrec[:-1] + [mrec[-2]] + [mrec[-1]]
        area_under_curve_y = mprec[:-1] + [0.0] + [mprec[-1]]
        ax[i].fill_between(area_under_curve_x, 0, area_under_curve_y, alpha=0.25, edgecolor='darkorange')

        ax[i].set_xlim([0.0,1.0])
        ax[i].set_ylim([0.0,1.05])
        

def showIMG_RESULT(no_images, no_class):
    """   
    Plot image with bounding box for models SSD&YOLOv3 and ground truth
    For this project pourpose obly - it's not general purpose function
    
    _ARGS
    no_images:   int        the number of images
    no_class:    int        the numaber of class in model
    
    _OUTPUTS
    images                  trifecta: ground truth + SSD prediction + YOLOv3 predicted
    """
    images_name = 'hard_hat_workers' + str(no_images) + '.png'
    print(images_name)
    txt_name = 'hard_hat_workers' + str(no_images) + '.txt'
    img_path = "./data/eval/images"
    
    # source of bounding box
    ground_truth_path = './data/eval/ground_truth'
    txt_gt = os.path.join(ground_truth_path, txt_name)
    with open(txt_gt, mode='r') as f:
        lines_gt = [line.rstrip() for line in f]
        
    model_SSD_path = f'./models/SSD_{no_class}/_predicted'
    txt_SSD = os.path.join(model_SSD_path, txt_name)
    with open(txt_SSD, mode='r') as f:
        lines_SSD = [line.rstrip() for line in f]
        
    model_YOLO_path = f'./models/YOLOv3_{no_class}/_predicted'
    txt_YOLO = os.path.join(model_YOLO_path, txt_name)
    with open(txt_YOLO , mode='r') as f:
        lines_YOLO = [line.rstrip() for line in f]

    tri_images = []
    
    # dict with colors for classes/names of model
    dict_col = {'helmet':'deeppink', 'head':'gold', 'person':'limegreen'}
    dict_title = {0:'ground truth', 1:'SSD', 2:'YOLOv3'}
    
    # create a rectangle with description 
    for i, lines in enumerate([lines_gt, lines_SSD, lines_YOLO]):
        img = Image.open(os.path.join(img_path, images_name))
        draw = ImageDraw.Draw(img)
        font = ImageFont.truetype(font="./data/object/arial.ttf", size=(img.size[0] + img.size[1]) // 75)
        font_t = ImageFont.truetype(font="./data/object/arial.ttf", size=(img.size[0] + img.size[1]) // 50)

        for bbox in lines:
            if len(bbox.split(' ')) == 6:
                lbl, prob0, *ls_val = bbox.split(' ')
                xmin, ymin, xmax, ymax = [int(v) for v in ls_val]
                prob = '%.0f' % (float(prob0)*100) + "%"
                lbl_text = ' ' + lbl + ' ' + prob
            else:
                lbl, *ls_val = bbox.split(' ')
                xmin, ymin, xmax, ymax = [int(v) for v in ls_val]
                lbl_text = ' ' + lbl
                prob0 = 1
                
            if float(prob0) < 0.5:
                continue

            draw.rectangle([xmin, ymin, xmax, ymax], outline=dict_col.get(lbl))
            text_size = draw.textsize(lbl_text, font=font)
            if ymin < text_size[1]:
                draw.rectangle(
                    [xmin, ymax + text_size[1], xmin + text_size[0], ymax],
                    fill=dict_col.get(lbl), width=3)
                draw.text((xmin, ymax), lbl_text, fill='black',
                          font=font)
            else:
                draw.rectangle(
                    [xmin, ymin - text_size[1], xmin + text_size[0], ymin],
                    fill=dict_col.get(lbl), width=3)
                draw.text((xmin, ymin - text_size[1]), lbl_text, fill='black',
                          font=font)
                
        title_text = dict_title[i]
        title_size = draw.textsize(title_text, font=font_t)
        draw.rectangle(
            [0, img.size[0] - title_size[1]-5, 0 + title_size[0]+5, img.size[0]],
            fill='black', width=3)
        draw.text((2.5, img.size[0] - title_size[1]-2.5), title_text, fill='white',
                  font=font_t)

            
        tri_images.append(img)

    # final trifecta 
    final_im = Image.new('RGB', (3*max(img.size[0], img.size[1])+20, max(img.size[0], img.size[1])))
    x_offset = 0
    for im in tri_images:
        final_im.paste(im, (x_offset,0))
        x_offset += im.size[0]+10
    
    # show
    display(final_im)
    
    
def to_POLY(x_set, y_set, deg=3):
    z = np.polyfit(x_set, y_set, deg)
    p = np.poly1d(z)
    return p(x_set).tolist()
    

def showLOSS(tab_src, model_type, no_step, ls_col):
    """   
    Plot image with bounding box for models SSD&YOLOv3 and ground truth
    For this project pourpose only- it's not general purpose function
    
    _ARGS
    tab_src:      pd.DataFrame   source data with columns [Steps, Value]
    model_type:   str	          part of plot title - type of model
    no_step:      list           list of point-step: [first n, which is 500th from end]
    ls_col:       list           list of colors to plot [main, help]
    
    _OUTPUTS
    images                  trifecta: total loss during all training proces
    			     + starting from 500th step & last 500steps
    """
    plt.figure(figsize=(15,3))
    plt.plot(tab_src['Step'], tab_src['Value'], color=ls_col[0])
    plt.grid(ls=':')
    plt.ylabel('TotalLoss value')
    plt.xlabel('Steps')
    plt.title('Value of total loss during traning for '+ str(model_type))

    fig, ax = plt.subplots(1, 2, figsize=(15,3), gridspec_kw={'width_ratios':[4, 1]})
    ax[0].plot(tab_src[tab_src['Step']>no_step[0]]['Step'], 
               tab_src[tab_src['Step']>no_step[0]]['Value'], color=ls_col[0])
    ax[0].plot(tab_src[tab_src['Step']>no_step[0]]['Step'],
               to_POLY(tab_src[tab_src['Step']>no_step[0]]['Step'], 
                         tab_src[tab_src['Step']>no_step[0]]['Value']), color=ls_col[1])
    ax[0].grid(ls=':')
    ax[0].set_ylabel('TotalLoss value')
    ax[0].set_xlabel('Steps')
    ax[0].set_title('Steps from 500 to last');
    ax[1].plot(tab_src[tab_src['Step']>no_step[1]]['Step'], 
               tab_src[tab_src['Step']>no_step[1]]['Value'], color=ls_col[0])
    ax[1].grid(ls=':')
    ax[1].set_ylabel('TotalLoss value')
    ax[1].set_xlabel('Steps')
    ax[1].set_title('Last 500 steps');    
