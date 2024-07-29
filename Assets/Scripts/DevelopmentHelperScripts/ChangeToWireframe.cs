using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Valve.VR;

public class ChangeToWireframe : MonoBehaviour
{
    public Shader shader;
    public Material material;
    Renderer rend;
    Material[] old;
    List<Shader> oldShader;
    int i;
    bool triggerSet = false;
    // Start is called before the first frame update
    void Start()
    {
        oldShader = new List<Shader>();
        rend = GetComponent<Renderer>();
        old = rend.materials;
        foreach (var item in old)
        {
            oldShader.Add(item.shader);
        }

    }

    // Update is called once per frame
    void Update()
    {

        if ((SteamVR_Actions.default_GrabPinch.GetStateDown(SteamVR_Input_Sources.Any) || Input.GetKeyDown(KeyCode.W)) && !NauseaScore.pauseHighCS)
            WireFrameActivated();
    }
    void WireFrameActivated()
        {
            triggerSet = true;
            i++;
            if (i % 2 == 1)
            {
                foreach (var item in rend.materials)
                {
                    item.shader = shader;
                }

            }
            else
            {
                int j = 0; ;
                foreach (var item in oldShader)
                {
                    rend.materials[j].shader = item;
                    j++;
                }
            }

        }
    }
