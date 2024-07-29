using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using static UnityEngine.Mathf;
using System.IO;
using SimpleBlurURP;  


public class LinearEquation : MonoBehaviour
{   [Tooltip("Toggle to disable autonomous movement")]
    public bool set_manually = false;
    [Range(0, 2 * PI)]
    public float currentTheta;



    public float minSpeed = 0.1f, maxSpeed = 2f;
    public readValues rv;
    public float speed = 1;
    public Vector3 lasttPosition;
    public Vector3 positionVector;
    public Vector3 velocityVector;

    public float newSpeed;
    public bool randomizeSpeedzones = false, swapSpeedzones = true, doubleSpeedzones = true;
    public float trackLength, trackLengthFactor;
    public float newTrackLengthFactor;

    public bool constant_speed;
    public float sinusPower = 1;
    [Space(20f)]
    [Tooltip("Readonly list for the start borders")]
    public float[] thetaValues;
    public const int thetaDivisions = 16;   //must be 2 to the power of something to clear singularities (16,32,64,...)
    [Tooltip("Define how close a position on the function should be to the wanted distance")]
    public float intervalThreshold = 0.00001f;
    [Header("When changing scale dont forget to create new List")]
    [Range(1,100)]
    public float scale = 1;
    public static float f_scaling = 1;
    [Tooltip("Toggle to create a new List from given parameters. Once created you can disable this bool.")]
    public bool createNewList = true;
    [Tooltip("Define where the file with the thetaValues will be saved, so it wont need to be generated every time")]
    public string filepath = "/positionList.csv";
    public bool showDebugs;
    bool slowDownOn;
    bool roundReset = false, trackLengthAdjusted = false;
    public static float transformHeight;
    Vector3 lastPos;
    int startBorder, endBorder;
    private float curVel;
    public float cosIndexTime;
    public float randomFactor = 1;
    public int swapSpeedzonesData = -1;
    private float initialHeight;
    private float rollerCoasterHeight = 1.8f;
    private float velocitySmooth = 0;

    //rollercoaster
    public float minHeight = 0;
    public float maxHeight = 1;

    //binned velocity
    [SerializeField] public bool isBinned = false;

    public float binCount = 8;

    //random acc and dec - freq
    public bool randomizeAcceleration = false;
    private float randomAcc;
    private float maxSpeedSecondFreq;
    private float trackLengthFactorSecondFreq;
    private float trackLengthFactorRange;
    public float cosX = 0f;
    private float timeRemaining = 0f;
    private bool experimentBegin = true;

    //head bobbing
    //public bool headBobbing;
    //public float bobAmount = 0.5f;
    //public float bobCoefficient = 1;
    //private float bobTimer;

    float areaDistance;
    public float metersTrav = 0f;
    private float speedXZ, speedXYZ, heightDer;

    // Update is called once per frame
    private void Awake()
    {
        transform.position = v2tov3(f_polar(currentTheta));
        lastPos = v2tov3(f_polar(currentTheta - 0.01f));
        //transform.position = (f_polar3D(currentTheta, rollerCoasterHeight));
        //lastPos = (f_polar3D(currentTheta - 0.01f, rollerCoasterHeight));
    }

    private void Start()
    {

        rv.InitReadValues();

        setTrackLength(true);

        f_scaling = scale;
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
        
        if(!constant_speed)
            speedSinus();

        maxSpeed = (trackLength / 10) - minSpeed;

        //set max height to be ratio of 20 
        maxHeight = trackLength / 20;

    }

    void FixedUpdate()
    {
        //Debug.LogError("maxHeight " + maxHeight);
        //Debug.LogError("maxSpeed " + maxSpeed);
        //Debug.LogError("TrackLength  " + trackLength);
        //Debug.LogError("trackLength / 20  " + trackLength / 20);

        maxHeight = trackLength / 20;

        positionVector = transform.position - lasttPosition;
        velocityVector = positionVector / Time.deltaTime;

        metersTrav += Vector3.Distance(lasttPosition, transform.position);

        newSpeed = velocityVector.magnitude;
        lasttPosition = transform.position;

        lastPos = transform.position;

        Vector3 speedXZVector = new Vector3(velocityVector.x, 0, velocityVector.z);
        speedXZ = speedXZVector.magnitude;

        if (swapSpeedzones)
        {
            swapSpeedzonesData = 1;
        }

        if (slowDownOn)
        {
            speed = SmoothDamp(speed, 0f, ref curVel, 2f);
            transformHeight = SmoothDamp(transformHeight, 0f, ref velocitySmooth, 2f);
        }
        else
        {
            if (!constant_speed)
                speedSinus();
        }

        if (set_manually)
            transform.position = v2tov3(f_polar(currentTheta));
        else
        {
            if (controls.gameStarted == true)
            {
                transform.position = v2tov3(getNextPosition(speed * Time.deltaTime));

                if (showDebugs)
                 Debug.Log("Moved with error rate of " + Abs((transform.position - lastPos).magnitude - (speed * Time.deltaTime))*1000+"mm");
            }
        }

        doRotation();
        GetComponent<readValues>().DoUpdate();

        //Set/adjust the tracklengthFactor after first round
        if (currentTheta >= Mathf.PI && !roundReset)
        {
            if (!trackLengthAdjusted)
            {
                trackLength = rv.metersTravelled;
                trackLengthAdjusted = true;
            }
            setTrackLength(false);
            roundReset = true;
            Debug.Log("Track length adjusted!");
            if (randomizeSpeedzones)
            {
               randomFactor = Random.Range(0.5f, 1.5f);
            }
        }
        if (roundReset == true && currentTheta < Mathf.PI)
        {
            roundReset = false;
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

    public static Vector2 f_polar(float t)
    {
        t = t %(2 * PI);
        
        float r = f_scaling * Sqrt(2 * Abs(Cos(2 * t)));
        Vector2 res =  new Vector2(r * Cos(t), r * Sin(t));
        
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
        thetaValues = new float[thetaDivisions/2];
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

    void setTrackLength(bool isStartForCosIndex)
    {
        //compute track lengths
        maxSpeed = (trackLength / 10) - minSpeed;
        maxSpeedSecondFreq = ((5 * trackLength) / 200) - minSpeed;

        //if(randomizeAcceleration)
        //{
        //    maxSpeed = Random.Range((trackLength / 10) - minSpeed, ((17 * trackLength) / 200) - minSpeed);
        //}

        trackLengthFactor = (Mathf.PI * 4) / ((trackLength) / ((minSpeed + maxSpeed) / 2));

        trackLengthFactorSecondFreq = (Mathf.PI * 4) / (trackLength / ((minSpeed + maxSpeedSecondFreq) / 2));

        if (isStartForCosIndex)
        {
            cosIndexTime = 0;
        }

    }
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

        // speed = -Cos(currentTheta * 4);  //[-1|1]
        // speed += 1; //[0|2]
        // speed /= 2; //[0|1]
        // speed = Pow(speed, sinusPower); //take to the power to make the slow parts shorter
        // speed = speed * (maxSpeed - minSpeed) + minSpeed;   //[min|max]

        if (controls.gameStarted)
        {
            cosIndexTime += Time.deltaTime;
        }
        else
            return;


        trackLengthFactorRange = trackLengthFactor / trackLengthFactorSecondFreq;

        if(randomizeAcceleration)
        {
            if (timeRemaining <= 0)
            {
                cosIndexTime = 0;

                randomAcc = Random.Range(1f, trackLengthFactorRange);
                cosX = (doubleSpeedzones ? 2 : 1) * trackLengthFactorSecondFreq * randomFactor * randomAcc;
                timeRemaining = (2 * Mathf.PI) / ((doubleSpeedzones ? 2 : 1) * trackLengthFactorSecondFreq * randomFactor * randomAcc);
                speed = ((swapSpeedzonesData * Mathf.Cos(cosX * cosIndexTime) + 1) / 2) * (maxSpeed - minSpeed) + minSpeed;
                
            }
            else
            {
                speed = ((swapSpeedzonesData * Mathf.Cos(cosX * cosIndexTime) + 1) / 2) * (maxSpeed - minSpeed) + minSpeed;
                timeRemaining -= Time.deltaTime;
            }
        }
     
        else
        {
            cosX = (doubleSpeedzones ? 2 : 1) * trackLengthFactor * randomFactor;
            //speed = ((swapSpeedzonesData * Mathf.Cos(cosX * cosIndexTime) + 1) / 2) * (maxSpeed - minSpeed) + minSpeed;
            //float speedXYZ = ((swapSpeedzonesData * (Mathf.Cos(cosX * cosIndexTime) + 1)) / 2) * (maxSpeed - minSpeed) + minSpeed; 

            float speedXYZ = ((swapSpeedzonesData * (Mathf.Cos(cosX * cosIndexTime) + 1)) / 2) * (maxSpeed - minSpeed) + minSpeed;

            //float bSin = ((swapSpeedzonesData * cosX * Mathf.Sin(cosX * cosIndexTime) + 1) / 2) * (maxHeight - minHeight);
            //float bSin = ((swapSpeedzonesData * (Mathf.Cos(cosX * cosIndexTime) + 1)) / 2) * (maxHeight - minHeight); 
            //float bSin = ((swapSpeedzonesData * cosX * (Mathf.Cos(cosX * cosIndexTime))) / 2) * (maxHeight - minHeight); 
            //float bSin = swapSpeedzonesData * cosX * Mathf.Sin(cosX * cosIndexTime) * 1/2 * (maxHeight - minHeight);
            //float minSpeedXYZ = Mathf.Sqrt(Mathf.Abs(minSpeed * minSpeed - bSin * bSin));
            //float maxSpeedXYZ = Mathf.Sqrt(Mathf.Abs(maxSpeed * maxSpeed - bSin * bSin));
            //speed = Mathf.Sqrt(Mathf.Abs(speedXYZ * speedXYZ - bSin * bSin));
            //speed = 5f;
            //float omega = (8f * Mathf.PI / trackLength) * 15f; //(maxSpeed + minSpeed) * (0.5f);
            //speedXYZ = 15f; //(maxSpeed - minSpeed) * (Mathf.Cos(omega * cosIndexTime) + 1f) * (0.5f) + minSpeed;
            //heightDer = (maxHeight - minHeight) * (0.5f) * omega * (Mathf.Sin(omega * cosIndexTime));
            //heightDer = (rollerCoasterHeight - ((maxHeight - minHeight) * (-Mathf.Cos(omega * cosIndexTime) + 1f) * (0.5f) + minHeight)) / Time.deltaTime;

            float max_x = f_polar(0).x;
            float max_y = -float.MaxValue;
            for (float t = 0; t <= 2 * Mathf.PI; t += 0.01f)
            {
                Vector2 v = f_polar(t);
                if (v.y > max_y)
                {
                    max_y = v.y;
                }
            }

            // delta distance by delta time
            heightDer = Mathf.Abs((rollerCoasterHeight - ((maxHeight - minHeight) * (-Mathf.Cos(transform.position.z * Mathf.PI / max_y) + 1) / 2 + minHeight))) / Time.deltaTime;
 
            // x as input for height
            // rollerCoasterHeight = (maxHeight - minHeight) * (-Mathf.Cos(transform.position.x * 2 * Mathf.PI / max_x) + 1) / 2 + minHeight;

            // z as input for height           
            rollerCoasterHeight = (maxHeight - minHeight) * (-Mathf.Cos(transform.position.z * Mathf.PI / max_y) + 1) / 2 + minHeight;

            //rollerCoasterHeight = swapSpeedzonesData * ( - Mathf.Cos( ((8f * Mathf.PI * (maxSpeed + minSpeed)) / (trackLength * 2f)) * cosIndexTime ) + 1) * (maxHeight - minHeight) * (0.5f) + minHeight;

            transformHeight = rollerCoasterHeight;   

            speed = Mathf.Sqrt(Mathf.Abs(speedXYZ * speedXYZ - heightDer * heightDer));
            //speed = swapSpeedzonesData * (maxSpeed - minSpeed) * (Mathf.Cos(transform.position.x * 2 * Mathf.PI / max_x) + 1) / 2 + minSpeed;
            //speed = (maxSpeed - minSpeed) * (Mathf.Cos(omega * cosIndexTime) + 1f) * (0.5f) + minSpeed;
            //speed = Mathf.Sqrt(Mathf.Abs(speedXYZ * speedXYZ - heightDer * heightDer)) * (Mathf.Cos(omega * cosIndexTime) + 1f) * (0.5f);
            //speed = 15f;
        }

        //speed = ((Mathf.Sin(Time.realtimeSinceStartup *((2 * Mathf.PI) / 19.5f)) + 1)/2)*7 ;// * (maxSpeed- minSpeed) + minSpeed;
        //float cosX = (doubleSpeedzones ? 2 : 1) * cosIndexTime * trackLengthFactor * randomFactor;
        //Debug.Log("tracklengthfactor " + trackLengthFactor);
        //speed = ((swapSpeedzonesData * Mathf.Cos(cosX) + 1)/2) * (maxSpeed- minSpeed) + minSpeed;


        float cosInverse = Mathf.Acos(((2 * (speed - minSpeed) / (maxSpeed - minSpeed)) / swapSpeedzonesData) - 1);

        if (isBinned)
        {
            for (int i = 0; i < 2 * binCount; i++)
            {
                float startRange = i * (Mathf.PI / binCount);
                float endRange = (i + 1) * (Mathf.PI / binCount);
                if (startRange < cosInverse && cosInverse < endRange)
                {
                    speed = ((swapSpeedzonesData * Mathf.Cos(startRange) + 1) / 2) * (maxSpeed - minSpeed) + minSpeed;
                    break;
                }
            }
        }

        // Debug.Log("CosIndexTime " + cosIndexTime + " speed " + speed);

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
        while(true)
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
        return f_polar(values[closer]);
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
        if (Abs(a-b) >= PI)
        {
            return ((a + 2*PI + b) / 2 ) % (2*PI);
        }
        return (a + b) / 2f;
    }

    private void OnDrawGizmos()
    {
        Gizmos.color = Color.red;
        Gizmos.DrawRay(transform.position, transform.forward);
    }

    static float distanceTo(float theta, Vector2 pos) {
        return Vector2.Distance(f_polar(theta), pos);
    }

    void doRotation()
    {
        Vector3 lookDir = transform.position - v2tov3(f_polar(currentTheta - 0.001f));
        transform.forward = lookDir;
       // transform.LookAt(transform.position + lookDir);
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