using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Valve.VR;
using UnityEngine.XR;

public class controls : MonoBehaviour
{
    public NauseaScore score;
    public GameObject PosGuides;
    public static bool gameStarted =false;
    public bool manualStart = false;
    public GameObject visualFlowObjects;
    public GameObject snow;
    public bool setResolutionFullHD;
    public bool wireFrameModeActive;

    // Update is called once per frame
    private void Start()
    {



    }

    void Update()
    {
        // Keyboard R to to decrease Resolution for better Performance/Debugging
        if (Input.GetKeyDown(KeyCode.R))
        {
            setResolutionFullHD = !setResolutionFullHD;
            XRSettings.eyeTextureResolutionScale = setResolutionFullHD ? 0.655f : 1;
        }
        // Increase NauseaScore Button Event
        if (SteamVR_Actions.default_upperButton.GetStateDown(SteamVR_Input_Sources.Any))
        {
            score.scoreUp(true);
            Debug.Log("up");
        }
        // Decrease NauseaScore Button Event
        if (SteamVR_Actions.default_lowerButton.GetStateDown(SteamVR_Input_Sources.Any))
        {
            score.scoreDown(true);
            Debug.Log("down");
        }
        if (SteamVR_Actions.default_upperButton.GetState(SteamVR_Input_Sources.Any))
        {
            score.scoreUp(false);
        }

        if (SteamVR_Actions.default_lowerButton.GetState(SteamVR_Input_Sources.Any))
        {
            score.scoreDown(false);
        }
        // Submit Questionaire
        if (SteamVR_Actions.default_lowerButton.GetStateUp(SteamVR_Input_Sources.Any) || SteamVR_Actions.default_upperButton.GetStateUp(SteamVR_Input_Sources.Any))
        {
            score.submit();
        }
        // Hold Triggerbutton or Space to start the movement
        if (SteamVR_Actions.default_GrabGrip.GetState(SteamVR_Input_Sources.Any) || Input.GetKey(KeyCode.Space))
        {
            Debug.Log("grip or space");
            score.start();  
        }
        // Reset the starttimer for accurate logging
        if (SteamVR_Actions.default_GrabGrip.GetStateUp(SteamVR_Input_Sources.Any))
        {
            score.resetStartTimer();
        } 
        if (SteamVR_Actions.default_GrabPinch.GetStateDown(SteamVR_Input_Sources.Any) || Input.GetKeyDown(KeyCode.W) && wireFrameModeActive)
        {
           // changeVisualFlow();
        }
        if (gameStarted)
        {
            PosGuides.SetActive(false);
        }
        if (manualStart)
            gameStarted = true;
    }
    void changeVisualFlow()
    {
        if (NauseaScore.pauseHighCS)
            return;
        visualFlowObjects.SetActive(!visualFlowObjects.activeSelf);
        snow.SetActive(!snow.activeSelf);
    }
}
