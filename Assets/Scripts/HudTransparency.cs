using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;
using TMPro;
public class HudTransparency : MonoBehaviour
{
    public Image slider;
    public Image sliderBG;
    public TextMeshProUGUI text;
    public TextMeshProUGUI score;

    public float transSpeed;
    public float transparencyTarget;
    public float cooldownMax;
    float value;
    float speed;
    float cooldown;
    // Start is called before the first frame update
    void Start()
    {
        value = transparencyTarget;   
    }

    // Update is called once per frame
    void Update()
    {
        // Smooth blendanimation of the UI
        cooldown -= Time.deltaTime;
        if(cooldown < 0)
        value = Mathf.SmoothDamp(value, transparencyTarget, ref speed, transSpeed);


        Color c = slider.color;
        c.a = value;
        slider.color = c;

        c = sliderBG.color;
        c.a = value;
        sliderBG.color = c;

        c = text.color;
        c.a = value;
        text.color= c;

        c = score.color;
        c.a = value;
        score.color = c;
    }

    public void reset()
    {
        cooldown = cooldownMax;
        value = 1;
    }
}
