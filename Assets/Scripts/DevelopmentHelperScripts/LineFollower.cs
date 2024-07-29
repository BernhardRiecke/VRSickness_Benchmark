using System.Collections;
using System.Collections.Generic;
using UnityEngine;

/*
 * this script follow the points of the line renderer attached to it
 * drag & drop the line renderer component to the field, or use GetComponent at start
 * disable script to stop from other script component
 */

public class LineFollower : MonoBehaviour
{

    public GameObject player;
    //Set speed
    [Range(0,1)]
    public float speedMultiplier = 1;
    public bool move = false;
    //Increasing value for lerp
    public float moveSpeed;
    //Linerenderer's position index
    int indexNum;
    public LineRenderer path;

    public bool manipulizeSpeed = false;


    Vector3 lastPos= new Vector3(0,0,0), velocity;
    public float velocityNorm;
    public float accelNorm;
    float lastVelocity;
    Vector3 nextLookTarget;
    Vector3 currentLookTarget;
    Vector3 rotationSpeed;
    public float difference;
    float rotation, rotationOld,sumRotation, sumRotationOld, rotAcc, rotAccOld, timeDiff;
    private void Start()
    {
        Debug.Log(System.DateTime.Now.ToString());
    }

    void Update()
    {
 
      // Export Data for LookRot
        
     
        velocity = transform.position - lastPos;
        lastPos = transform.position;
        velocityNorm = velocity.magnitude;
        accelNorm = velocityNorm - lastVelocity;
        //func = Mathf.Cos(moveSpeed/path.positionCount * 2 * Mathf.PI) * Data.scale * Mathf.Sin(moveSpeed / path.positionCount * 2 * 2 * Mathf.PI) * Data.scale / Data.ScaleFactor;

        if (manipulizeSpeed)
        speedMultiplier = (Mathf.Sin(moveSpeed/path.positionCount*2*Mathf.PI*Data.speedzoneFrequency - (Data.moveSpeedzones * Mathf.PI))+1)/2;
           
        if (move)
        {
            difference = (Data.minSpeed + speedMultiplier * ((Data.maxSpeed) - Data.minSpeed));// ( 1 + Mathf.Sin(moveSpeed)*Mathf.Sin(moveSpeed*2));
            moveSpeed += difference; //Mathf.Sin(moveSpeed);
            moveSpeed = modulo(moveSpeed, path.positionCount);
        }

        player.transform.position = getLookTargetFromSinus(moveSpeed, path.positionCount);

        //smooth Rotation
        transform.LookAt(getLookTargetFromSinus(moveSpeed+0.1f, path.positionCount));

        lastVelocity = velocityNorm;
    }

    float modulo(float i, float m)
    {
        if (i < 0) return modulo(i + m,m);
        if (i >= path.positionCount) return modulo(i - m,m);
        return i;
    }
    Vector3 getLookTargetFromSinus(float pos,int maxPos)
    {
        float p = pos / maxPos * 2 * Mathf.PI;
        float x, y, z;
        y = transform.position.y;
        x = Mathf.Sin(p) * Data.scale;
        z = Mathf.Sin(p * 2) * Data.scale / Data.ScaleFactor;

        return new Vector3(x, y, z);
    }

    private void OnDrawGizmos()
    {
        Gizmos.color = Color.red;
        Gizmos.DrawSphere(getLookTargetFromSinus(moveSpeed + 0.1f, path.positionCount), 0.1f);
    }
    public float ComputeRotData(string accVel)
    {
        if (accVel == "acc")
        {
            return rotAcc;
        }
        rotationOld = rotation;
        sumRotationOld = sumRotation;
        rotAccOld = rotAcc;
        if (transform.eulerAngles.y > 180)
         {
            rotation = (transform.eulerAngles.y - 360) * 10;
         }
        else
         { 
            rotation = transform.eulerAngles.y * 10; // *10 to get degree per sec
         }

        sumRotation = rotation - rotationOld;
        rotAcc = sumRotation - sumRotationOld;

        if (rotAcc > 500 || sumRotation > 500)
          {
            rotAcc = rotAccOld;
            sumRotation = sumRotationOld;
          }
        
        if (accVel == "vel")
        {
            return sumRotation;
        }
        else return 0;

    }
}