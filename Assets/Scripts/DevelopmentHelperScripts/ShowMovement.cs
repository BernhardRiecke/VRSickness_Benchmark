using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ShowMovement : MonoBehaviour
{
    public Transform target;
    public float rate, minDistance;
    LineRenderer lr;
    float timer = -1;
    Vector3 lastPoint;
    // Start is called before the first frame update
    void Start()
    {
        lr = GetComponent<LineRenderer>();
        lastPoint = target.position;
    }

    // Update is called once per frame
    void Update()
    {
        
        if (timer > rate && Vector3.Distance(lastPoint, target.position) > minDistance)
        {
            timer = 0;
            //change to flying mode
            //lastPoint = target.position;
            lr.positionCount++;
            //change to flying mode
            lr.SetPosition(lr.positionCount-1, lastPoint);
            
        }
        timer = Mathf.Max(0, timer);
        timer += Time.deltaTime;
    }
}
