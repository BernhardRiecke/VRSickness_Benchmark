using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Valve.VR;
public class change_to_Wireframe_Mat : MonoBehaviour
{
    Material standart;
    public Material wireframe;

    bool set = true;
    Renderer rend;

    private void Start()
    {
        
        rend = GetComponent<Renderer>();
        standart = rend.material;
    }
    // Update is called once per frame
    void Update()
    {
        if ((SteamVR_Actions.default_GrabPinch.GetStateDown(SteamVR_Input_Sources.Any) || Input.GetKeyDown(KeyCode.W)) && !NauseaScore.pauseHighCS)
            WireFrameActivated();
    }

    void WireFrameActivated()
    {
        if (set)
            rend.material = wireframe;
        else
            rend.material = standart;

        set = !set;

    }
}
