using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class readValues : MonoBehaviour
{
    public float metersTravelled = 0;
    public float metersTravelledForHeight = 0;
    public float secondsTravelled = 0;
    public float meter_per_sec, accel = 0;
    float rotation = 357.113f*10, rotationOld, sumRotation, sumRotationOld, rotAcc, oldMS = 0f;
    float msnotnull,accelnotnull;
    Vector3 lastPos;
    bool init;
    LinearEquation linear;
    public void InitReadValues()
    {
        lastPos = transform.position;
        linear = GetComponent<LinearEquation>();
    }

    public void DoUpdate()
    {
        if (controls.gameStarted)
        {
            if (!init) 
            {
            InitReadValues();
                init = true;
            }

            float m = (v3tov2(lastPos) - v3tov2(transform.position)).magnitude;
            float mForHeight = ((lastPos) - (transform.position)).magnitude;
            float s = Time.fixedDeltaTime;
            metersTravelled += mForHeight;
            metersTravelledForHeight += mForHeight;
            secondsTravelled += s;
            meter_per_sec = mForHeight / s;

            if (meter_per_sec != 0)
                msnotnull = meter_per_sec;

            accel = (meter_per_sec - oldMS);
            if (accel != 0)
                accelnotnull = accel;
            oldMS = msnotnull;
            lastPos = transform.position;
   
        }

    
    }

    public static Vector2 v3tov2(Vector3 v)
    {
        return new Vector2(v.x, v.z);
    }

    /*
     * Not used anymore!
     * 
     * 
    public float ComputeRotData(string accVel)
    {
        if (accVel == "acc")
        {
            return rotAcc;
        }

        rotationOld = rotation;
        sumRotationOld = sumRotation;

        if (transform.eulerAngles.y > 180)
        {
            rotation = transform.eulerAngles.y * 10 - 360;
        }
        else
        { rotation = transform.eulerAngles.y * 10; }

        sumRotation = rotation - rotationOld;

        rotAcc = sumRotation - sumRotationOld;

        if (accVel == "vel")
        {
            return sumRotation;
        }
        else return 0;
    

}
     */
    public float getMS()
    {
        return Mathf.Min(Mathf.Max(linear.minSpeed,msnotnull), linear.maxSpeed);
    }
    public float getAcc()
    {
        return accelnotnull;
    }
    
   
}