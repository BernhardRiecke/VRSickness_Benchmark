using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Pause : MonoBehaviour
{
    float intensity, curVel, curVel2;
    Light[] lights, neonlights;
    float neonlightIntensity, lightsIntensity;
    public GameObject sun, snow, neonLights;
    bool m_slowDown;

    // Start is called before the first frame update
    void Start()
    {
        lights = sun.GetComponentsInChildren<Light>();
        lightsIntensity = lights[0].intensity;
        neonlights = neonLights.GetComponentsInChildren<Light>();
        neonlightIntensity = neonlights[0].intensity;
        
    }

    // Update is called once per frame
    void Update()
    {


        if (m_slowDown) 
        { 
        intensity = Mathf.SmoothDamp(intensity, 1f, ref curVel, 3f);
            foreach (var item in lights)
            {
                item.intensity = intensity;
            }


            neonlightIntensity = Mathf.SmoothDamp(neonlightIntensity, 0f, ref curVel2, 3f);
            foreach (var item in neonlights)
            {
                item.intensity = neonlightIntensity;
            }
        }
        else
        {
            intensity = Mathf.SmoothDamp(intensity, 0f, ref curVel, 3f);
            foreach (var item in lights)
            {
                item.intensity = intensity;
            }


            neonlightIntensity = Mathf.SmoothDamp(neonlightIntensity, 1f, ref curVel2, 3f);
            foreach (var item in neonlights)
            {
                item.intensity = neonlightIntensity;
            }

        }


    }


    public void PauseMovement(bool slowDown)
    {
        m_slowDown = slowDown;
        if (slowDown)
        {
            snow.SetActive(false);
        }
        else
        {
            snow.SetActive(true);
        }
    }
}
