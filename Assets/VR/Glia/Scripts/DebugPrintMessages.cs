// (c) Copyright 2019-2021 HP Development Company, L.P.

using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using HP.Omnicept;
using HP.Omnicept.Messaging;
using HP.Omnicept.Messaging.Messages;

public class DebugPrintMessages : MonoBehaviour
{
    [SerializeField]
    private bool showHeartRateMessages = true;
    [SerializeField]
    private bool showHeartRateVariabilityMessages = true;
    [SerializeField]
    private bool showPPGMessages = true;
    [SerializeField]
    private bool showCognitiveLoadMessages = true;
    [SerializeField]
    private bool showEyeTrackingMessages = true;
    [SerializeField]
    private bool showVsyncMessages = true;
    [SerializeField]
    private bool showCameraImageMessages = true;
    [SerializeField]
    private bool showCameraImageTexture = true;
    [SerializeField]
    private bool showIMUMessages = true;
    [SerializeField]
    private bool showSubscriptionResultListMessages = true;

    public Material cameraImageMat;
    private Texture2D cameraImageTex2D;

    public void Start()
    {
        cameraImageTex2D = new Texture2D(400, 400, TextureFormat.R8, false);
        if (cameraImageMat != null)
        {
            cameraImageMat.mainTexture = cameraImageTex2D;
        }
    }

    public void OnDestroy()
    {
        Destroy(cameraImageTex2D);
    }

    public void HeartRateHandler(HeartRate hr)
    {
        if (showHeartRateMessages && hr != null)
        {
            Debug.Log(hr);
        }
    }

    public void HeartRateVariabilityHandler(HeartRateVariability hrv)
    {
        if (showHeartRateVariabilityMessages && hrv != null)
        {
            Debug.Log(hrv);
        }
    }

    public void PPGFrameHandler(PPGFrame ppg)
    {
        if (showPPGMessages && ppg != null)
        {
            Debug.Log(ppg);
        }
    }

    public void CognitiveLoadHandler(CognitiveLoad cl)
    {
        if (showCognitiveLoadMessages && cl != null)
        {
            Debug.Log(cl);
        }
    }

    public void EyeTrackingHandler(EyeTracking eyeTracking)
    {
        if (showEyeTrackingMessages && eyeTracking != null)
        {
            Debug.Log(eyeTracking);
        }
    }

    public void VSyncHandler(VSync vsync)
    {
        if (showVsyncMessages && vsync != null)
        {
            Debug.Log(vsync);
        }
    }

    public void CameraImageHandler(CameraImage cameraImage)
    {
        if (cameraImage != null)
        {
            if (showCameraImageMessages)
            {
                Debug.Log(cameraImage);
            }
            if (showCameraImageTexture && cameraImageMat != null && cameraImage.SensorInfo.Location == "Mouth")
            {
                // Load data into the texture and upload it to the GPU.
                cameraImageTex2D.LoadRawTextureData(cameraImage.ImageData);
                cameraImageTex2D.Apply();
            }
        }
    }

    public void IMUFrameHandler(IMUFrame imu)
    {
        if (showIMUMessages && imu != null)
        {
            Debug.Log(imu);
        }
    }

    public void DisconnectHandler(string msg)
    {
        Debug.Log("Disconnected: " + msg);
    }

    public void ConnectionFailureHandler(HP.Omnicept.Errors.ClientHandshakeError error)
    {
        Debug.Log("Failed to connect: " + error);
    }

    public void SubscriptionResultListHandler(SubscriptionResultList SRLmsg)
    {
        if (showSubscriptionResultListMessages && SRLmsg != null)
        {
            Debug.Log(SRLmsg);
        }
    }
}