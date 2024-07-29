using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Figure8Track : MonoBehaviour
{

    public float a = 10;
    public float b = 5;
    public float c = 1;
    public float frequency = 0.2f;
    public int numPoints = 1000;

    public Vector3[] points;

    private void Start()
    {
        points = new Vector3[numPoints];
        for (int i = 0; i < numPoints; i++)
        {
            float t = 2 * Mathf.PI * i / numPoints;
            float x = (a + b * Mathf.Cos(t)) * Mathf.Cos(t);
            float y = (a + b * Mathf.Cos(t)) * Mathf.Sin(t);
            float z = c * Mathf.Sin(frequency * t);
            points[i] = new Vector3(x, y, z);
        }
    }
}