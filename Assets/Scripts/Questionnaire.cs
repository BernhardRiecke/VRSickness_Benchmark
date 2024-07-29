using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;
using TMPro;

public class Questionnaire : MonoBehaviour
{
    public Slider vectionIntensity, presence;
    public TextMeshProUGUI vectionPct, presencePct;

    private void Start()
    {
        presence.value = 5;
    }

    private void Update()
    {
        vectionPct.text = (vectionIntensity.value * 5).ToString();
        presencePct.text = (presence.value * 5).ToString();
    }

    public void submit()
    {
        NauseaScore.vectionIntensityValue = (int) vectionIntensity.value * 5;
        NauseaScore.presenceValue = (int) presence.value * 5;
        gameObject.SetActive(false);
        Debug.Log("SUBMITTED");
    }

    public void increaseScore (Slider slider)
    {
        slider.value += 1;
    }
    public void decreaseScore(Slider slider)
    {
        slider.value -= 1;
        
    }

}
