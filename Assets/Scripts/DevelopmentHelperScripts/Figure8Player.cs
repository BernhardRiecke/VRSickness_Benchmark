using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Figure8Player : MonoBehaviour
{

    public Figure8Track track;
    public float speed = 1;
    private int currentPoint = 0;

    private void Update()
    {
        transform.position = track.points[currentPoint];
        currentPoint = (currentPoint + 1) % track.numPoints;
    }
}