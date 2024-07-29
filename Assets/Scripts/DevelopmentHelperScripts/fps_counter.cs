using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class fps_counter : MonoBehaviour
{
    public int fps;
    public float avg_fps;

    List<int> fpss;
    private void Start()
    {
        fpss = new List<int>();
    }
    // Update is called once per frame
    void LateUpdate()
    {
        if (Time.time > 1)
        {
            if (fpss.Count > 500)
            {
                fpss.RemoveAt(0);
            }
            
            fps = (int)(1.0f / Time.deltaTime);
            fpss.Add(fps);
            avg_fps = 0;
            foreach(int i in fpss)
            {
                avg_fps += i;
            }
            avg_fps /= fpss.Count;
            avg_fps = (int) avg_fps;
        }
    }

    void OnGUI()
    {
        GUI.Label(new Rect(0, 0, 100, 100), fps.ToString());
        GUI.Label(new Rect(0, 100, 100, 100), avg_fps.ToString());
    }
}
