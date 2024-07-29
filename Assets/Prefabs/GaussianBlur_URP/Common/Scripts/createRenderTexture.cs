/*
createRenderTexture.cs
this creates a newCamera and uses it to update a RenderTexture.
This RenderTexture will be passed to the blur to be used for the scene.
*/

using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class createRenderTexture : MonoBehaviour
{
    public Camera templateCamera;
    public LayerMask excludeLayer;
    private Camera newCamera;
    public RenderTexture thisRenderTexture;


    // Start is called before the first frame update
    void Start()
    {
        if (!templateCamera)
        {
            templateCamera = Camera.main;
        }

        newCamera = Instantiate(templateCamera);

        newCamera.transform.parent = templateCamera.transform.parent;
        newCamera.transform.localPosition = templateCamera.transform.localPosition;
        newCamera.transform.localRotation = templateCamera.transform.localRotation;
        newCamera.transform.localScale = templateCamera.transform.localScale;

        newCamera.cullingMask = ~excludeLayer;

        newCamera.targetTexture = thisRenderTexture;
    }

    // Update is called once per frame
    void Update()
    {
        
    }
}
