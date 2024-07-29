 GaussianBlur_URP
-------------------------------------
[Asset Store Link](http://u3d.as/1wQD)  
[HRP version](http://u3d.as/1EMR)  
[non-ShaderGraph version](http://u3d.as/yJk)  

© 2022 Justin Garza

PLEASE LEAVE A REVIEW OR RATE THE PACKAGE IF YOU FIND IT USEFUL!
Enjoy! :)


## Table of Contents

<!--TOC-->
* [GaussianBlur_URP](#gaussianblur_urp)
	* [Table of Contents](#table-of-contents)
	* [Contact](#contact)
	* [Terms of Use](#terms-of-use)
	* [Description Features](#description-features)
	* [Set Up](#set-up)
	* [Set Up (for Transparent objects)](#set-up-(for-transparent-objects))
	* [GaussianBlur_IU](#gaussianblur_iu)
	* [GaussianBlur_IUEffect](#gaussianblur_iueffect)
	* [GaussianBlur_WS](#gaussianblur_ws)
	* [Demo_AdvancedUI_1.0 and Demo_AdvancedUI_2.0](#demo_advancedui_1.0-and-demo_advancedui_2.0)
	* [Videos](#videos)
	* [FAQs](#faqs)
		* [why can't i see Sprites in the Blur?](#why-can't-i-see-sprites-in-the-blur?)
		* [it's just GREY!](#it's-just-grey!)
		* [Can I layer multiple Blur objects?](#can-i-layer-multiple-blur-objects?)
		* [better performance from mobile](#better-performance-from-mobile)

<!--TOC-->

## Contact

My Contact info is on my github profile  
[https://github.com/jgarza9788](https://github.com/jgarza9788)
 
## Terms of Use

Required:
please follow [Unity's EULA](https://unity3d.com/legal/as_terms) 

Suggestion/Optional:
please put my name in the credits, or in the special thanks section. :)  

## Description Features

* Alpha Mask
* Mobile Friendly
* Adjust Blur, Lightness, Saturation, and TintColor 
* Built on URP
* easily modifiable/editable Shader Graph
* 3 Shaders
    * GaussianBlur_IU
    * GaussianBlur_IUEffect
    * GaussianBlur_WS (WorldSpace, or Objects)

## Set Up
You need to set up your project to use a URP Asset.  
1. You can use the URL_Asset i provided (in ***\GaussianBlur_URP\Assets\URP**)  
[Image Link](https://i.imgur.com/B7s0p8L.png)
2. or use your own.  
if you use your own check ✅ "Opaque Texture"  
[Image Link](https://imgur.com/a/WvZ4e4H.png)
    * if you are making a mobile game consider changing the "opaque Downsampling" option.   

Note:  
"Opaque Texture" gives us the ability to access _CameraOpaqueTexture, a texture of what the main camera sees (but this texture will not have transparent objects (like Sprites) within it)

## Set Up (for Transparent objects)
if you need sprites and non-opaque to show up in the blur, please see the **Demo_Basic(Sprites).scene**

[Image Link](https://imgur.com/a/RE5Ao69.png)

1. the RTCamera outputs an image to a RenderTexture 
    *  *(most of the time you'll want this to view exactly what the main Camera sees)* 
2. the Shader uses the RenderTexture as the based image for the blur.

You can actual use nearly any image you want.  
So if you don't want to use a second camera, consider a static image.  

In addition, you can change the Size and Color Format of your RenderTexture.  
reducing the size and optimizing it.


## GaussianBlur_IU
To use the shader on the UI, just use the material.  
(or create you're own material and use my shader)

CustomTexture:  
pass in any texture (image) you'd like (i.e. a Render Texture or 2D Texture)

useCameraOpaqueTexture:  
this will be used as a default texture for blurring.  
note: this will not render transparent (Sprite and non-Opaque) objects (you might want to use a Render Texture)
* see **Set Up (for Transparent objects)** section above

BlurScale:  
This is how much to blur the texture.  
(This will automatically adjust two other variables)

Lightness:  
how light or dark the UI should be.

Tint:  
a color tint to be applied to the UI.

Saturation:  
adjusts the saturation (color) of the UI.

![Image Link](https://i.imgur.com/CPsRJI8.png)


## GaussianBlur_IUEffect
This will use the Source Image.  
So you can fade the blur depending on where it is on the screen.

see **Demo_UIEffect.scene**:  
The Source Image is our map to know how much to blur the edges and the center.  
![Image Link](https://i.imgur.com/vZ7FJoN.png)


## GaussianBlur_WS
This is for Objects in the WorldSpace.  
see **Demo_3DModel.scene** for an example.

note:  
The Layer should be set to BlurObject.

Metallic & Smoothness:  
these are used to adjust the shinny-ness of the object.  

if this is not working we might want to double check our custom-renderer. 
it's in ***\GaussianBlur_URP\Assets\URP**

![Image Link](https://i.imgur.com/X4vxYgk.png)  

Here are the settings for the custom-renderer.
1. we filter out the Layer "BlurObject"
    * because we do not want the camera to render it.
2. we create a render feature to add the BlurObject back into the final image after being rendered my the GaussianBlur_WS material.
![Image Link](https://i.imgur.com/isubyX3.png)


## Demo_AdvancedUI_1.0 and Demo_AdvancedUI_2.0
these scenes shows how we can blur only a piece of the image.  
In the ShaderGraph we are creating a mask.

we are controlling the about of blur and other items with shader properties and Animator Component.

![Image Link](https://imgur.com/cD0K34c.png)


## Videos
these videos go in deal on how the ShaderGraphs work.  
please watch these if you plan to create your own remix of the shader.  

[GaussianBlur_UI Video](https://youtu.be/v11TBFgPKDE)  
[GaussianBlur_WS Video](https://youtu.be/lwK_AaKw4kc)    
[GaussianBlur_UIEffect Video](https://youtu.be/2delOzh9Wt8)


## FAQs

### why can't i see Sprites in the Blur?
by default by Shader will be using the _CameraOpaqueTexture, this texture only contains Opaque Objects. However we can use whatever Texture we want.

please read the **Set Up (for Transparent objects)** section above.


### it's just GREY!
Most of the time this is just caused by the settings in the URP_Asset or the settings you picked in the shader.

please review the settings and make sure they are ok.  

Try re-downloading the asset and/or Contact me for help.


### Can I layer multiple Blur objects?
this asset can be adjusted for multiple Blur Layers, however there are a few issues with adjusting this asset for Blurred Layers.

1. i like to keep this asset simple enough so most people can easily use it.
2. multiple blur layers can be very taxing on the CPU/GPU
3. we would need to have multiple cameras, render textures, etc (one for each layer) or have a special shader to merge and blur each.


### better performance from mobile 

1. use Vulkan 
    * this should be in the PlayerSettings, Other Settings, Rendering.
2. DownSampling
    * you can also change the "Opaque Downsampling" option in the URP_Asset.
    * reduce the size and color format of the render texture.  

