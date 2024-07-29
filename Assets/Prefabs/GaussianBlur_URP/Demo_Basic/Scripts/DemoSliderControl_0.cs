//This Script just joins the value of the silders to the correct blur and lightness values in the materials.
//this script can help you design your own control script for your game.

using UnityEngine;
using UnityEngine.UI;
using System.Collections;

public class DemoSliderControl_0 : MonoBehaviour
{

    //public Text IterationsText;
    //public Slider Iterations;

    //public Text KernelText;
    //public Slider Kernel;

    public Text BlurScaleText;
    public Slider BlurScale;

    public Text LightnessText;
    public Slider Lightness;

    public Text SaturationText;
    public Slider Saturation;

    public Text TintColorText;
    public Slider TintColor;
    public Image Slider;
    public Image Handle;

    public Gradient TintGradient;

    public Material BlurMaterial;

    // Update is called once per frame
    void FixedUpdate() 
    {

        //IterationsText.text = "Iterations: " + Iterations.value.ToString("");
        //KernelText.text = "Kernel: " + Kernel.value.ToString("");

        //BlurScaleText.text = "BlurScale: " + BlurScale.value.ToString("");
        //LightnessText.text = "Lightness: " + Lightness.value.ToString("F3");
        //SaturationText.text = "Saturation: " + Saturation.value.ToString("F3");

        //if (TintColor.value == 0f)
        //{
        //    TintColorText.text = "TintColor: Off";
        //}
        //else
        //{
        //    TintColorText.text = "TintColor:";
        //}

        Color thisColor = TintGradient.Evaluate(TintColor.value);
        Slider.color = thisColor;
        Handle.color = thisColor;

        //UpdateRateText.text = "UpdateRate: " + UpdateRate.value.ToString("F3");

        //BRM.Iterations = (int)Iterations.value;
        //BRM.DownRes = (int)DownRes.value;
        //BRM.UpdateBlur = UpdateBlur.isOn;
        //BRM.UpdateRate = UpdateRate.value;

        BlurMaterial.SetFloat("_BlurScale", BlurScale.value);
        BlurMaterial.SetFloat("_Saturation", Saturation.value);
        BlurMaterial.SetFloat("_Lightness", Lightness.value);
        BlurMaterial.SetColor("_Tint", thisColor);

    }

    //public void ReRender()
    //{
    //    BR.RenderBlur(MaxBlur.value, (int)Quality.value, 5, (int)DownRes.value, true);
    //}


        
}

