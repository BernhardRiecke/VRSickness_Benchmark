using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using static UnityEngine.Mathf;
using System.IO;

public class ThetaVis : MonoBehaviour
{
    public enum format
    {
        circle,
        clover,
        eight
    }
    public bool increase = false;
    public format form;
    [Tooltip("Toggle to disable autonomous movement")]
    public bool set_manually = false;
    public static float currentTheta;


    public float minSpeed = 0.1f, maxSpeed = 2f;
    public float speed = 1;
    public bool constant_speed;
    public float sinusPower = 1;
    [Space(20f)]
    [Tooltip("Readonly list for the start borders")]
    public float[] thetaValues;
    public const int thetaDivisions = 16;   //must be 2 to the power of something to clear singularities (16,32,64,...)
    [Tooltip("Define how close a position on the function should be to the wanted distance")]
    public float intervalThreshold = 0.00001f;
    [Header("When changing scale dont forget to create new List")]
    public float f_scaling = 1;
    [Tooltip("Toggle to create a new List from given parameters. Once created you can disable this bool.")]
    public bool createNewList = true;
    [Tooltip("Define where the file with the thetaValues will be saved, so it wont need to be generated every time")]
    public string filepath = "/Markus Method/positionList.csv";
    public bool showDebugs;
    bool slowDownOn;

    static float transformHeight;
    Vector3 lastPos;
    int startBorder, endBorder;
    private float curVel;


    private void Start()
    {

        transformHeight = transform.position.y;
        filepath = Application.dataPath + filepath;
        if (createNewList)
        {
            createCSVList();
        }
        if (thetaValues.Length == 0)
        {
            thetaValues = CSVtoArray(filepath);
        }
        transform.position = v2tov3(func(form, currentTheta));
        lastPos = v2tov3(func(form, currentTheta-0.1f));

        if (!constant_speed)
            speedSinus();
    }


    void FixedUpdate()
    {

        lastPos = transform.position;

        if (slowDownOn)
        {
            speed = SmoothDamp(speed, 0f, ref curVel, 2f);
        }
        else
        {
            if (!constant_speed)
                speedSinus();
        }

        if (set_manually)
            transform.position = v2tov3(func(form, currentTheta));
        else
        {

            if (increase)
                transform.position = v2tov3(getNextPosition(speed * Time.deltaTime));
            else
                transform.position = v2tov3(func(form, currentTheta));
        }

        doRotation();
        GetComponent<readValues>().DoUpdate();
    }

    public Vector2 func(format f,float t)
    {
        switch (f)
        {
            case format.circle:
                return circle(t);
            case format.clover:
                return clover(t);
            default:
                return f_polar(t);
        }
    }

    #region Figure Function

    public static Vector3 v2tov3(Vector2 v)
    {
        return new Vector3(v.x, transformHeight, v.y);
    }

    public static Vector2 v3tov2(Vector3 v)
    {
        return new Vector2(v.x, v.z);
    }

    //function for the figure 8
    public Vector2 f_polar(float t)
    {
        t = t % (2 * PI);

        float r = f_scaling * Sqrt(2 * Abs(Cos(2 * t)));
        Vector2 res = new Vector2(r * Cos(t), r * Sin(t));

        if (t < PI * 1 / 4)
            return res;
        if (t < PI * 3 / 4)
            return new Vector2(-res.y, -res.x);
        if (t < PI * 5 / 4)
            return new Vector2(-res.x, -res.y);
        if (t < PI * 7 / 4)
            return new Vector2(res.y, res.x);
        return res;

    }
    public Vector2 clover(float t)
    {
        t = t % (2 * PI);

        float r = f_scaling * Sqrt(2 * Abs(Cos(2 * t)));
        Vector2 res = new Vector2(r * Cos(t), r * Sin(t));
        return res;

    }
    public Vector2 circle(float t)
    {
        t = t % (2 * PI);

        Vector2 res = new Vector2(f_scaling * Cos(t), f_scaling * Sin(t));

        return res;

    }

    //function for a circle around zero pos
    public static Vector2 g_polar(float t, float r = 1)
    {
        //nothing special here, just a circle with radius r
        return new Vector2(r * Cos(t), r * Sin(t));
    }

    #endregion

    #region CSV Theta List 
    public void createCSVList()
    {
        //create list with singularities as border for intervals
        thetaValues = new float[thetaDivisions / 2];
        int writeIdx = 0;
        for (int i = 0; i < thetaDivisions; i++)
        {
            if (Cos(2 * i * PI * 2 / thetaDivisions) >= 0)
                thetaValues[writeIdx] = i * PI * 2 / thetaDivisions;
            else
                writeIdx--;
            writeIdx++;
        }
        ArrayToCSV(thetaValues, filepath);
        thetaValues = new float[0];
    }

    public static float[] CSVtoArray(string path, char seperator = ';')
    {
        string[] lines = File.ReadAllLines(path);
        Debug.Log("Read " + lines.Length + " positions from " + path);
        float[] data = new float[lines.Length];

        for (int i = 0; i < lines.Length; i++)
        {
            data[i] = float.Parse(lines[i]);
        }

        return data;
    }

    public static void ArrayToCSV(float[] data, string path, char seperator = ';')
    {
        string[] contents = new string[data.Length];

        Debug.Log("Write " + contents.Length + " positions to " + path);
        if (File.Exists(path))
            File.Delete(path);
        StreamWriter stream = File.CreateText(path);
        new FileInfo(path).IsReadOnly = false;

        for (int i = 0; i < data.Length; i++)
        {
            stream.WriteLine(data[i].ToString());
        }

        stream.Close();
    }

    #endregion


    void speedSinus()
    {
        /*  pos	    theta	speed
            -----------------
            right 	0	    min
            mid	    pi*1/4	max
            left	pi*2/4	min
            mid	    pi*3/4	max
            right	pi	    min
            mid	    pi*5/4	max
            left	pi*6/4	min
            mid	    pi*7/4	max
            right 	2*pi	min
        */

        speed = -Cos(currentTheta * 4);  //[-1|1]
        speed += 1; //[0|2]
        speed /= 2; //[0|1]
        speed = Pow(speed, sinusPower); //take to the power to make the slow parts shorter
        speed = speed * (maxSpeed - minSpeed) + minSpeed;   //[min|max]
    }

    public Vector2 getNextPosition(float speed)
    {
        int counter = 0;
        //find fitting start interval

        float[] values = new float[3];
        float[] dist = new float[3];
        int lastUnder = idxOfLastUnder(currentTheta, thetaValues);
        startBorder = lastUnder;
        values[0] = thetaValues[lastUnder];
        values[2] = getUpperBorder(thetaValues, v3tov2(transform.position), speed, lastUnder);
        values[1] = avg(values[0], values[2]);
        if (showDebugs)
        {
            Debug.Log("New Position get calculated\t" + currentTheta + " \t-------------------------------------------------------------------------------------------------------------------------");
            Debug.Log("Start Thetas [" + values[0] * 1000 + "\t|" + values[1] * 1000 + "\t|" + values[2] * 1000 + "]");
        }

        //binary search - dividing interval into 2 parts recursively
        while (true)
        {
            if (showDebugs)
                Debug.Log("Try #" + counter);
            values[1] = (avg(values[0], values[2])); //calc middle theta

            //calc distances for each interval step
            for (int i = 0; i < dist.Length; i++)
            {
                dist[i] = distanceTo(values[i], v3tov2(transform.position)); //distances to current pos
                if (isBiggerTheta(currentTheta, values[i]))
                    dist[i] *= -1;

                dist[i] = -(speed - dist[i]); //distance to wanted speed
            }
            //sucess & fail criteria
            counter++;
            if (counter > 200 || dist[1] == dist[2] || dist[1] == dist[0] || Abs(dist[1]) <= intervalThreshold || Abs(dist[2]) <= intervalThreshold)
            {
                break;
            }

            // new values for next iteration
            //(all n) (2n 1p) (1n 2p) (all p)
            if (dist[1] > 0)
            {//(1n 2p) (all p)
                if (dist[0] > 0)
                    throw new System.Exception("ERROR: all positives. Start Must be lower");
                values[2] = values[1];
            }
            else
            {//(all n) (2n 1p)
                if (dist[2] < 0)
                    throw new System.Exception("ERROR: all negatives. Start must be higher");
                values[0] = values[1];
            }
            values[1] = avg(values[0], values[2]);
            if (showDebugs)
            {
                Debug.Log("New Thetas [" + values[0] * 1000 + "\t|" + values[1] * 1000 + "\t|" + values[2] * 1000 + "]");
                Debug.Log("thesh: " + intervalThreshold * 1000 + " [" + dist[0] * 1000 + "\t|" + dist[1] * 1000 + "\t|" + dist[2] * 1000 + "\t]");
            }
        }

        //found theta that leads to position within epsilon <= interval threshold
        int closer = Abs(dist[1]) < Abs(dist[2]) ? 1 : 2;
        currentTheta = values[closer];
        return func(form, values[closer]);
    }

    public float getUpperBorder(float[] values, Vector2 currentPos, float speed, int lastUnderidx)
    {


        for (int i = 0; i < values.Length; i++)
        {
            int index = (i + lastUnderidx) % values.Length;
            if (isBiggerTheta(currentTheta, values[index]))
                continue;
            if (distanceTo(values[index], currentPos) <= speed)
                continue;
            endBorder = index;
            return values[index];

        }
        throw new System.Exception("ERROR: in finding new upper start border");

    }

    public static float avg(float a, float b)
    {
        if (Abs(a - b) >= PI)
        {
            return ((a + 2 * PI + b) / 2) % (2 * PI);
        }
        return (a + b) / 2f;
    }

    private void OnDrawGizmos()
    {
        Gizmos.color = Color.red;
        Gizmos.DrawRay(transform.position, transform.forward);
    }

    float distanceTo(float theta, Vector2 pos)
    {
        return Vector2.Distance(func(form, theta), pos);
    }

    void doRotation()
    {
        Vector3 lookDir = transform.position - lastPos;

        transform.LookAt(transform.position + lookDir);
    }

    static bool isBiggerTheta(float a, float b)
    {
        return Abs(a - b) >= PI ? !(a > b) : (a > b);
    }

    int idxOfLastUnder(float theta, float[] list)
    {
        int idx = list.Length - 1; ;
        for (int i = 0; i < list.Length; i++)
        {
            if (list[i] > theta)
                return idx;
            idx = i;

        }
        return idx;
    }

    public void slowDown(bool On)
    {
        slowDownOn = On;
    }


}