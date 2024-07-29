using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ColorGradient : MonoBehaviour
{
    LineRenderer colorLine;
    public Gradient gradient;
    public LineRenderer track;

    private void OnEnable()
    {
        if (Data.ShowSpeedIndication)
        {
            colorLine = GetComponent<LineRenderer>();
            colorLine.positionCount = track.positionCount;

            for (int i = 0; i < track.positionCount; i++)
            {
                colorLine.SetPosition(i, track.GetPosition(i) + new Vector3(0, 0.005f, 0));
            }


            Texture2D texture = new Texture2D(Data.SpeedPoints.Count * 2, 1, TextureFormat.RGB24, false);
            texture.SetPixel(0, 0, Data.moveSpeedzones > 1 ? Color.green : Color.red);
            texture.SetPixel((Data.SpeedPoints.Count * 2) - 1, 0, Data.moveSpeedzones > 1 ? Color.green : Color.red);

            for (int i = 0; i < (Data.SpeedPoints.Count * 2) - 2; i++)
            {
                texture.SetPixel(i + 1, 0, (Data.moveSpeedzones > 1 ? i : i + 2) % 4 < 2 ? Color.red : Color.green);
            }

            texture.Apply();
            colorLine.GetComponent<Renderer>().material.mainTexture = texture;
        }
    }


    // Update is called once per frame
    void Update()
    {
        
    }
    
    Vector3 sinFigure8(float pos)
    {
        float x, y, z;

        y = transform.position.y;
        x = Mathf.Sin(pos) * Data.scale;
        z = Mathf.Sin(pos * 2) * Data.scale / Data.ScaleFactor;

        return new Vector3(x, y, z);
    }
}
