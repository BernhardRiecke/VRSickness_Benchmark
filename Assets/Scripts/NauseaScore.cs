using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using TMPro;
using UnityEngine;
using UnityEngine.Rendering;
using UnityEngine.Rendering.Universal;
using UnityEngine.UI;
using Valve.VR;


//Script for Logging Data und Triggering various Events
public class NauseaScore : MonoBehaviour
{

    //using glia behaviour to access heart rate data
    //private GliaBehaviour _gliaBehaviour = null;
    //private GliaBehaviour gliaBehaviour
    //{
    //    get
    //    {
    //        if (_gliaBehaviour == null)
    //        {
    //            _gliaBehaviour = FindObjectOfType<GliaBehaviour>();
    //        }
    //        return _gliaBehaviour;
    //    }
    //}

    public enum ControlMode
    {
        passive,
        virtual_active,
        physical_active
    }

    public enum BodySwayMode
    {
        none,
        movement,
        eyes_open_pre,
        eyes_closed_pre,
        eyes_open_post,
        eyes_closed_post
    }

    private string SubjectName;
    private string SubjectGender = "female";
    private string SubjectAge = "19";
    int currentNauseaScore;
    [HideInInspector]
    public float time, time2, waitTime, waitTime2 = 3, timeElapsed;
    public string enviromentSize = "S", MotionSicknessReductionMethod = "none", condition, locoMotionInterface = "passive";
    public ControlMode translationMode = ControlMode.passive, rotationMode = ControlMode.passive;
    public bool vignetteOn;
    [Range(10f, 70f)]
    float Vignette = 10f;

    [Range(10f, 100f)]
    public float desiredFOV = 10f;
    public bool AutoFOV;
    [Range(10f, 100f)]
    public float MaxFOV = 10f;
    public bool DynamicBlur;
    public bool VirtualNose;
    public Color noseColor;
    public bool blurEffect;

    public Image imageElement;

    public Renderer quadRenderer;



    [Header("NauseaScore termination conditions")]
    public int CSAbortValue = 101;
    public int NauseaScoreContinue;
    [Space(20f)]
    public GameObject vignette;
    public TextMeshProUGUI text;
    public Image slider;
    public HudTransparency hud;
    public GameObject instructions;
    public GameObject score;
    public Image fillbar;
    public readValues playerLine;
    public GameObject player;
    public GameObject snow;
    public GameObject blackPanel;
    private float bodySwayTimeLeft = 60f;
    private int trialNumber = 0;
    private bool startedOnce = false;
    private string heartRateNum = "0";
    public GameObject nose;
    public Material noseMat;

    //public Camera mainCamera;
    //public Camera blurCamera;
    //public RenderTexture renderTexture;

    //public Material blurMaterial;
    public float blurIntensity = 1.0f;

    private BodySwayMode bodySwayCondition = BodySwayMode.none;

    [Header("Excel in US-Format")]
    public bool usFormat;
    [Space(20f)]
    bool started = false;
    int rowAmount = 0;
    int trial, preTrialCS;
    bool counted = false;
    bool summaryBool = false;
    public static bool pauseHighCS = false;
    public static int presenceValue, vectionIntensityValue;
    bool triggerd = false;
    int lowestNauseaScore = 100, highestNauseaScore = 0;
    string MinCSTime = "0", MaxCSTime = "0", HeadsetName;
    public GameObject questionnaire;
    public LinearEquation linearEquation;
    public Pause pause;
    float oldMS, rotationOld, rotationVelOld;
    int count = 0;
    private string MScheck;
    float scale_factor;

    float FOVFactor;
    public Volume PPV;
    DepthOfField dph;

    float desiredFOVRad;

    // Start is called before the first frame update
    void Start()
    {
        readPID();
        Application.targetFrameRate = 90;
        oldMS = linearEquation.minSpeed;
        presenceValue = -1;
        vectionIntensityValue = -1;
        Data.name = SubjectName;
        Data.gender = SubjectGender;
        Data.age = SubjectAge;
        Data.envSize = enviromentSize;
        Data.locoMoInterface = locoMotionInterface;
        Data.translationMode = translationMode;
        Data.rotationMode = rotationMode;
        Data.MSreductionMethod = MotionSicknessReductionMethod;
        Data.condition = condition;
        text.text = "0%";
        //playerLine.move = false;


        // Get VR-Headsetname for Log
        try
        {
            HeadsetName = SteamVR.instance.hmd_ModelNumber;

        }
        catch (System.Exception)
        {
            HeadsetName = "No Headset";
        }

        int i = 0;
        while (i <= 100)
        {
            PlayerPrefs.DeleteKey(i.ToString());
            i = i + 5;
        }


        
    }
    


    // Update is called once per frame
    void FixedUpdate()
    {
        updateNose();

        if (DynamicBlur)
        {
            computeDynamicBlur();
        }
  
        imageElement.gameObject.SetActive(blurEffect);

        if (AutoFOV)
        {
            FOVFactor = (80 - desiredFOV) / linearEquation.maxSpeed;
            MaxFOV = 80 - linearEquation.speed * FOVFactor;
        }
        else
        {
            MaxFOV = desiredFOV;
        }
        
        float desiredFOVRad = MaxFOV * Mathf.Deg2Rad;

        if (blurEffect)
        {
            //blurCamera.transform.position = mainCamera.transform.position;
            //blurMaterial.SetFloat("_BlurAmount", blurIntensity);
        }
//=======
        //desiredFOVRad = desiredFOV * Mathf.Deg2Rad;
//>>>>>>> Stashed changes
        // 285.7 = (1/radius in scale of one) = (1/0.0035)
        // 0.331 is the distance between eye and scaling image
        scale_factor = (0.331f * Mathf.Tan(desiredFOVRad / 2)) * 285.7f;

        // Debug.Log("scale factor" + scale_factor + "desired" + desiredFOV);

        vignette.transform.localScale += new Vector3(scale_factor, scale_factor, scale_factor);
        Vignette = scale_factor;

        ChangeVignette();

        //Show the bodysway condition to the experimentor 
        Debug.Log("Current bodysway condition: " + bodySwayCondition);

        if (pauseHighCS)
        {
            PauseHighCS();
        }

        if (Input.GetKeyDown(KeyCode.F))
        {
            vignetteOn = !vignetteOn;
        }
        if (Input.GetKeyDown(KeyCode.UpArrow))
            Vignette = Mathf.Min(29, Vignette + 1);
        if (Input.GetKeyDown(KeyCode.DownArrow))
            Vignette = Mathf.Max(11, Vignette - 1);

        //Start Logging
        if (controls.gameStarted == true)
        {
            if (!summaryBool)
            {
                summaryBool = true;
                Summary(Data.name);
                StartCoroutine(CreateCSV(Data.name));
            }
            if ((timeElapsed >= rowAmount * 0.01f) && (HeadsetName != "No Headset"))
            {
                //Debug.Log(Time.fixedDeltaTime);
                //trialNumber = 1;
                CreateCSVSensoryData(Data.name);
                //CreateCSVTrajectoryData(Data.name);

            }
        }
        else
        {
            if (!counted)
            {
                if (PlayerPrefs.GetString("DataName") == Data.name)
                {
                    trial = PlayerPrefs.GetInt("Trial");
                    //Debug.Log(PlayerPrefs.GetString("DataName") + " " + Data.name + " " + trial);
                    trial++;
                }
                else
                {
                    PlayerPrefs.SetString("DataName", Data.name);
                    trial = 1;
                }

                PlayerPrefs.SetInt("Trial", trial);
                counted = true;
                //Summary(Data.name);

            }
            CreateCSVSensoryData(Data.name);
        }

        timeElapsed += Time.deltaTime;

        if (slider != null)
        {
            slider.fillAmount = (float)currentNauseaScore / 100;
        }

        if (fillbar != null)
            fillbar.fillAmount = (float)time / waitTime2;

        //indicate eyes closed and open conditions 
        if (Input.GetKeyDown(KeyCode.Alpha1))
        {
            if (!startedOnce)
            {
                bodySwayCondition = BodySwayMode.eyes_closed_pre;
                bodySwayTimeLeft = 60f;
            }
            else
            {
                bodySwayCondition = BodySwayMode.eyes_closed_post;
                bodySwayTimeLeft = 60f;
            }
        }
        else if (Input.GetKeyDown(KeyCode.Alpha2))
        {
            if (!startedOnce)
            {
                bodySwayCondition = BodySwayMode.eyes_open_pre;
                bodySwayTimeLeft = 60f;
            }
            else
            {
                bodySwayCondition = BodySwayMode.eyes_open_post;
                bodySwayTimeLeft = 60f;
            }
        }
        else if (Input.GetKeyDown(KeyCode.Alpha0))
        {
            bodySwayCondition = BodySwayMode.none;
        }
        else if (Input.GetKeyDown(KeyCode.Alpha3))
        {
            bodySwayCondition = BodySwayMode.movement;
        }

        //turn screen black when eyes are closed and start the counter 
        if (bodySwayCondition == BodySwayMode.eyes_closed_pre || bodySwayCondition == BodySwayMode.eyes_closed_post)
        {
            blackPanel.SetActive(true);
            bodySwayTimeLeft -= Time.deltaTime;
            if (bodySwayTimeLeft < 0)
            {
                bodySwayCondition = BodySwayMode.none;
            }
        }
        else if (bodySwayCondition == BodySwayMode.eyes_open_pre || bodySwayCondition == BodySwayMode.eyes_open_post)
        {
            blackPanel.SetActive(false);
            bodySwayTimeLeft -= Time.deltaTime;
            if (bodySwayTimeLeft < 0)
            {
                bodySwayCondition = BodySwayMode.none;
            }
        }
        else
        {
            blackPanel.SetActive(false);
        }

        //manual bodysway measurement
        if(Input.GetKeyDown(KeyCode.B))
        {
            pauseHighCS = true;
            linearEquation.slowDown(true);
            pause.PauseMovement(true);
            triggerd = true;
            questionnaire.SetActive(false);
            bodySwayCondition = BodySwayMode.none;
        }

        if (Input.GetKey("escape")) {
        //#if UNITY_EDITOR
        //    EditorApplication.isPlaying = false;
            Application.Quit();
        }

    }

    public void updateNose()
    {
        noseMat.color = noseColor;
        nose.SetActive(VirtualNose);
    }

     public void computeDynamicBlur()
    {
       if (PPV.profile.TryGet<DepthOfField>(out dph))
      {
         float value = 30 / linearEquation.maxSpeed;
         dph.focalLength.value = value * linearEquation.speed;
         }
    }


    //function to map a range to another range - used for the vignette
    public float MapRange(float value, float originalMin, float originalMax, float newMin, float newMax)
    {
        return (value - originalMin) * (newMax - newMin) / (originalMax - originalMin) + newMin;
    }


    //Creating Logging Data
    public IEnumerator CreateCSV(string DataName)
    {

        string root = Application.persistentDataPath + "/" + DataName + "_Trial" + trial + ".csv";


        if (!File.Exists(root))
        {

            var sr = File.CreateText(root);
            string dataCSV = "Date and Time" + "," + "Unix Time" + "," + "Trial" + "," + "Participant ID" + "," + "Environment Size" + "," + "Motionsickness Reduction Method," + "Time" + "," + "Max Speed (m/s)" + "," + "MoveSpeedZones" + "," + "Speedzone Frequency" + "," + "Nausea Rating," + "Vignette";

            sr.WriteLine(dataCSV);

            FileInfo fInfo = new FileInfo(root);
            fInfo.IsReadOnly = false;
            

            sr.Close();
            StartCoroutine(CreateCSV(Data.name));
        }

        else
        {

            //mapped vignette to percentage scale
            //float VignetteMapped = MapRange(Vignette, 10, 70, 10, 100);

            System.DateTime epochStart = new System.DateTime(1970, 1, 1, 0, 0, 0, 0, System.DateTimeKind.Utc);
            long cur_time = (long)(System.DateTime.UtcNow - epochStart).TotalMilliseconds;

            ArrayList d = getData();
            string dataCSV =
                                System.DateTime.Now.ToString("MM/dd/yyyy HH:mm:ss") + ";" +
                                cur_time + ";" +
                                trial + " ;" +
                                Data.name + ";" +
                                Data.envSize + " ;" +
                                Data.MSreductionMethod + ";" +
                                //Data.age + ";" +
                                //Data.gender + ";" +
                                //timeElapsed.ToString("0.00") + ";" +
                                playerLine.secondsTravelled + ";" +
                                d[0] + ";" +
                                d[1] + ";" +
                                d[2] + ";" +
                                currentNauseaScore + ";" +
                                MaxFOV + ";"
                                //VignetteMapped + ";"
                                ;

            dataCSV += System.Environment.NewLine;

            //DE-Format
            // dataCSV = dataCSV.Replace('.', ',');

            //US-Format
            dataCSV = dataCSV.Replace(';', ',');


            File.AppendAllText(root, dataCSV);
            FileInfo fInfo = new FileInfo(root);
            fInfo.IsReadOnly = false;

        }

        yield return new WaitForSeconds(0.1f);
    }

    //Change Vignette Max and Min Vignette, needs to be adjusted to Headset in Settings
    public void ChangeVignette()
    {
        if (vignetteOn)
        {
            vignette.SetActive(true);
            vignette.transform.localScale = Vector3.one * Vignette;  //9.6 - 29.1
            Data.MSreductionMethod = "FOV Reduction";
        }
        else
        {
            vignette.SetActive(false);

            if(DynamicBlur)
            {
                Data.MSreductionMethod = "DynamicBlur";
            }
            else if(VirtualNose)
            {
                Data.MSreductionMethod = "Virtual Nose";
            }
            else if(blurEffect)
            {
                Data.MSreductionMethod = "Blur";
            }
            else {
                Data.MSreductionMethod = "None";
            }
            
        }
    }

    //Create LogData of Sensory Data
    public void CreateCSVSensoryData(string DataName)
    {

        string date = System.DateTime.Now.ToString();
        date = date.Replace(":", "-");
        date = date.Replace(".", "_");
        string root = Application.persistentDataPath + "/" + "sensoryData_" + DataName + "_" + Data.envSize + "_Trial_" + trial + ".csv";


        if (!File.Exists(root))
        {

            var sr = File.CreateText(root);
            string dataCSV = "Date and Time," + "Unix Timestamp," + "Participant ID," +
              "Trial," + "Eviroment Size," + "Body Sway Condition," +
              "Nausea Score," +
              "Locomotion Interface," + "Translation Mode," + "Rotation Mode," +
              "Visualization Hardware" + "," + "Motionsickness Reduction Method," + "CS Abortion Value," +
              "Time (Beginning)," + "Distance/Meters Travelled," + "Seconds Travelled," + "Vignette," +
           //   "Heart Rate," 

           // + "left Eye Gaze X," + "left Eye Gaze Y," + "left Eye Gaze Z," + "left Eye Gaze Confidence," + "left Eye Pupil Position X," + "left Eye Pupil Position Y," + "left Eye Pupil Position Confidence," + "left Eye Openness," + "left Eye Openness Confidence," + "left Eye Pupil Dilation," + "left Eye Pupil Dilation Confidence,"
           // + "right Eye Gaze X," + "right Eye Gaze Y," + "right Eye Gaze Z," + "right Eye Gaze Confidence," + "right Eye Pupil Position X," + "right Eye Pupil Position Y," + "right Eye Pupil Position Confidence," + "right Eye Openness," + "right Eye Openness Confidence," + "right Eye Pupil Dilation," + "right Eye Pupil Dilation Confidence,"

             "X," + "Z," + "Y," + "Yaw," + "Velocity (m/s)," + "Acceleration," + "Yaw Angle," + "Yaw Velocity (°/s)," + "Yaw Acceleration," 
           // "Cognitive Load Value," + "Cognitive Load Standard Deviation," + "Cognitive Load Data State," + 
           // "Accelerometer X," + "Accelerometer Y," + "Accelerometer Z," + "Gyroscope X," + "Gyroscope Y," + "Gyroscope Z,"
            ;



            if (!usFormat)
                dataCSV = dataCSV.Replace(",", ";");
            //PostTrial CS

            preTrialCS = currentNauseaScore;


            sr.WriteLine(dataCSV);
            FileInfo fInfo = new FileInfo(root);
            fInfo.IsReadOnly = false;


            sr.Close();

        }

        else
        {

            //Compute export data for Summary

            if (currentNauseaScore < lowestNauseaScore)
            {
                lowestNauseaScore = currentNauseaScore;
                MinCSTime = timeElapsed.ToString("0.000");
            }
            else if (currentNauseaScore > highestNauseaScore)
            {
                highestNauseaScore = currentNauseaScore;
                MaxCSTime = timeElapsed.ToString("0.000");
            }
            if (PlayerPrefs.GetFloat(currentNauseaScore.ToString()) == 0)
                // FLAG = this is where i changed the % time 
                PlayerPrefs.SetFloat(currentNauseaScore.ToString(), playerLine.secondsTravelled);

            float MS = playerLine.getMS();
            float rotation = RotationPlayer();
            float rotationVel = rotation - rotationOld;
            float rotationAcc = rotationVel - rotationVelOld;

            //Biometric data

            //Heart Rate
            //string heartRate = gliaBehaviour.GetLastHeartRate() + "";
            //Debug.Log("Heart rate" + gliaBehaviour.GetLastHeartRate());

            //System.Text.RegularExpressions.Match regex;
            //regex = System.Text.RegularExpressions.Regex.Match(heartRate, @"[+-]?([0-9]*[.])?[0-9]+");

            //if (regex.Success)
            //    heartRateNum = System.Text.RegularExpressions.Regex.Match(heartRate, @"[+-]?([0-9]*[.])?[0-9]+").Value;
         
            //Eye tracking

            //string eyeTracking = gliaBehaviour.GetLastEyeTracking() + "";
            //string[] eyeTrackingSplit = eyeTracking.Split('=');
            //string eyeTrackingTest = eyeTracking;

            //List<string> listStrLineElements = new List<string>();

            //regex = System.Text.RegularExpressions.Regex.Match(eyeTrackingTest, @"[+-]?([0-9]*[.])?[0-9]+");

            // while (regex.NextMatch().Success)
            // {
            //    eyeTrackingTest = eyeTrackingTest.Substring(regex.Index + regex.Value.Length);
            //    listStrLineElements.Add(regex.Value);
            //    regex = System.Text.RegularExpressions.Regex.Match(eyeTrackingTest, @"[+-]?([0-9]*[.])?[0-9]+");
            //}

            //if(listStrLineElements.Count == 0)
            //{
            //    for (int i = 0; i < 22; i++)
            //    {
            //        listStrLineElements.Add("0");

            //    }
            //}
            
            //Left eye 
            ///*string leftEyeGazeX = listStrLineElements[0];
            //string leftEyeGazeY = listStrLineElements[1];
            //string leftEyeGazeZ = listStrLineElements[2];
            //string leftEyeGazeConfidence = listStrLineElements[3];

            //string leftEyePupilPositionX = listStrLineElements[4];
            //string leftEyePupilPositionY = listStrLineElements[5];
            //string leftEyePupilPositionConfidence = listStrLineElements[6];

            //string leftEyeOpennessOpenness = listStrLineElements[7];
            //string leftEyeOpennessConfidence = listStrLineElements[8];

            //string leftEyePupilDilationPupilDilation = listStrLineElements[9];
            //string leftEyePupilDilationConfidence = listStrLineElements[10];

            ////Right eye 
            //string rightEyeGazeX = listStrLineElements[11];
            //string rightEyeGazeY = listStrLineElements[12];
            //string rightEyeGazeZ = listStrLineElements[13];
            //string rightEyeGazeConfidence = listStrLineElements[14];

            //string rightEyePupilPositionX = listStrLineElements[15];
            //string rightEyePupilPositionY = listStrLineElements[16];
            //string rightEyePupilPositionConfidence = listStrLineElements[17];

            //string rightEyeOpennessOpenness = listStrLineElements[18];
            //string rightEyeOpennessConfidence = listStrLineElements[19];

            //string rightEyePupilDilationPupilDilation = listStrLineElements[20];
            //string rightEyePupilDilationConfidence = listStrLineElements[21];*/

            System.DateTime epochStart = new System.DateTime(1970, 1, 1, 0, 0, 0, 0, System.DateTimeKind.Utc);
            long cur_time = (long)(System.DateTime.UtcNow - epochStart).TotalMilliseconds;

            //cognitive load
            //string cognitiveLoad = gliaBehaviour.GetLastCognitiveLoad() + "";
            //regex = System.Text.RegularExpressions.Regex.Match(cognitiveLoad, @"[+-]?([0-9]*[.])?[0-9]+");
            //string cognitiveLoadValue, cognitiveLoadStandardDeviation, cognitiveLoadDataState;
            //if (regex.Success)
            //{
            //    cognitiveLoadValue = regex.Value;
            //    cognitiveLoad = cognitiveLoad.Substring(regex.Index + regex.Value.Length);
            //    regex = System.Text.RegularExpressions.Regex.Match(cognitiveLoad, @"[+-]?([0-9]*[.])?[0-9]+");
            //    cognitiveLoadStandardDeviation = regex.Value;
            //    cognitiveLoad = cognitiveLoad.Substring(regex.Index + regex.Value.Length);
            //    regex = System.Text.RegularExpressions.Regex.Match(cognitiveLoad, @"[+-]?([0-9]*[.])?[0-9]+");
            //    cognitiveLoadDataState = regex.Value;
            //}
            //else
            //{
            //    cognitiveLoadValue = "0.0";
            //    cognitiveLoadStandardDeviation = "0.0";
            //    cognitiveLoadDataState = "0.0";
            //}

            //IMU data
            //string IMUFrame = gliaBehaviour.GetLastIMUFrame() + "";
            //List<string> IMU3AxisValues = new List<string>();
            //for (int i = 0; i < 6; i++)
            //{
            //    regex = System.Text.RegularExpressions.Regex.Match(IMUFrame, @"[+-]?([0-9]*[.])?[0-9]+");
            //    IMUFrame = IMUFrame.Substring(regex.Index + regex.Value.Length);
            //    IMU3AxisValues.Add(regex.Value);
            //}
            //string accX = IMU3AxisValues[0];
            //string accY = IMU3AxisValues[1];
            //string accZ = IMU3AxisValues[2];
            //string gyroX = IMU3AxisValues[3];
            //string gyroY = IMU3AxisValues[4];
            //string gyroZ = IMU3AxisValues[5];

            //mapped vignette to percentage scale
            // VignetteMapped = MapRange(Vignette, 10, 70, 10, 100);

            //if (IMU3AxisValues.Count == 0)
            //{
            //    for (int i = 0; i < 6; i++)
            //    {
            //        IMU3AxisValues.Add("0");
            //    }
            //}


            if (linearEquation.isBinned)
            {
                MScheck = (Mathf.Floor(MS)).ToString("0.0000");
            }
            else
            {
                MScheck = MS.ToString("0.0000");
            }

            //long cur_time = (long) DateTimeOffset.Now.ToUnixTimeMilliseconds();
            string dataCSV =
                System.DateTime.Now.ToString("MM/dd/yyyy HH:mm:ss") + " ;" +
                cur_time + ";" +
                DataName + " ;" +
                trial + " ;" +
                Data.envSize + " ;" + //only one size at the moment
                bodySwayCondition + ";" +
                currentNauseaScore + ";" +
                Data.locoMoInterface + " ;" + // only one interface at the moment
                Data.translationMode + ";" +
                Data.rotationMode + ";" +
                HeadsetName + ";" +
                Data.MSreductionMethod + ";" +
                CSAbortValue + ";" +
                timeElapsed.ToString("0.0000") + ";" +
                playerLine.metersTravelled + ";" +
                playerLine.secondsTravelled + ";" +
                MaxFOV + ";" +
                //heartRateNum + ";" +
                //leftEyeGazeX + ";" +
                //leftEyeGazeY + ";" +
                //leftEyeGazeZ + ";" +
                //leftEyeGazeConfidence + ";" +
                //leftEyePupilPositionX + ";" +
                //leftEyePupilPositionY + ";" +
                //leftEyePupilPositionConfidence + ";" +
                //leftEyeOpennessOpenness + ";" +
                //leftEyeOpennessConfidence + ";" +
                //leftEyePupilDilationPupilDilation + ";" +
                //leftEyePupilDilationConfidence + ";" +
                //rightEyeGazeX + ";" +
                //rightEyeGazeY + ";" +
                //rightEyeGazeZ + ";" +
                //rightEyeGazeConfidence + ";" +
                //rightEyePupilPositionX + ";" +
                //rightEyePupilPositionY + ";" +
                //rightEyePupilPositionConfidence + ";" +
                //rightEyeOpennessOpenness + ";" +
                //rightEyeOpennessConfidence + ";" +
                //rightEyePupilDilationPupilDilation + ";" +
                //rightEyePupilDilationConfidence + ";" +
                player.transform.position.x.ToString("0.000") + ";" +
                player.transform.position.z.ToString("0.000") + ";" +
                player.transform.position.y.ToString("0.000") + ";" +
                player.transform.rotation.eulerAngles.y.ToString("0.000") + ";" +
                MScheck + ";" +
                (oldMS - MS).ToString("0.0000") + ";" +
                RotationPlayer().ToString("0.00") + ";" +
                (rotationVel * 10).ToString("0.00") + ";" +
                (rotationAcc * 10).ToString("0.00") + ";" 
                //cognitiveLoadValue + ";" +
                //cognitiveLoadStandardDeviation + ";" +
                //cognitiveLoadDataState + ";" +
                //accX + ";" +
                //accY + ";" +
                //accZ + ";" +
                //gyroX + ";" +
                //gyroY + ";" +
                //gyroZ + ";"
                ;
            ;


            oldMS = MS;
            rotationOld = rotation;
            rotationVelOld = rotationVel;
            count++;


            dataCSV += System.Environment.NewLine;
            if (usFormat)
            {
                dataCSV = dataCSV.Replace(',', '.');
                dataCSV = dataCSV.Replace(';', ',');
            }
            else
            {
                dataCSV = dataCSV.Replace('.', ',');
            }


            File.AppendAllText(root, dataCSV);
            FileInfo fInfo = new FileInfo(root);
            fInfo.IsReadOnly = false;
            rowAmount = (int)(timeElapsed) + 1;

        }

        //Abort App when CS is too strong

        if (currentNauseaScore >= CSAbortValue && !triggerd)
        {
            pauseHighCS = true;
            Debug.Log("Paused cause to too high Nausea Score");
            linearEquation.slowDown(true);
            pause.PauseMovement(true);
            triggerd = true;
            questionnaire.SetActive(false);
            bodySwayCondition = BodySwayMode.none;
        }

    }

    public void readPID()
    {
        try
        {
            string rootR = Application.persistentDataPath + "/participant_id.txt";

            using (StreamReader streamReader = new StreamReader(rootR))
            {
                SubjectName = streamReader.ReadToEnd();
                streamReader.Close();
            }

        }
        catch (Exception)
        {
            Debug.Log("create and fill participant_id.txt in %Appdata%/LocalLow/H-BRS/Snowflakes if you want to use participant IDs in your Logfile");
            throw;
        }
      
    }


    

    //Pause Study
    public void PauseHighCS()
    {

        if (currentNauseaScore == NauseaScoreContinue && trialNumber > 1)
        {
            Debug.Log("Start movement again");
            pauseHighCS = false;
            linearEquation.slowDown(false);
            pause.PauseMovement(false);
            triggerd = false;
            questionnaire.SetActive(false);

            /*
            EditorApplication.isPlaying = false;
                Application.Quit();
            */
        }

    }
    //Create Summary Log
    public void Summary(string DataName)
    {
        string date = System.DateTime.Now.ToString();
        date = date.Replace(":", "-");
        date = date.Replace(".", "_");
        string root = Application.persistentDataPath + "/" + "SummaryData_" + DataName + "_" + Data.envSize  + "_Trial-" + trial + ".csv";    //Maybe change this to be just "SummaryData" so all entries are in one .csv ?




        if (!File.Exists(root))
        {

            var sr = File.CreateText(root);
            string dataCSV = "Date and Time" + "," + "Subject" + "," + "Trial" + "," + "EviromentSize" + "," + "Locomotion Interface" + "," + "Translation Mode" + "," + "Rotation Mode" + "," + "Visualization Hardware" + "," + "Motionsickness Reduction Method" + "," + "Condition" + "," + "CS Abortion Value" + "," + "CS Abort Time," + "Pre-Trial CS Score," + "Min CS score," + "Min CS score time," + "Max CS score," + "Max CS score time," + "Range CS score," + "Vection Intensity," + "Presence,";

            for (int i = 5; i <= 100; i += 5)
            {
                dataCSV += "Time of " + i + "% CS Score" + ",";
            }
            sr.WriteLine(dataCSV);

            if (usFormat)
                dataCSV = dataCSV.Replace(";", ",");


            sr.Close();



        }

        else
        {
            int i = 5;
            string dataCSV =
                                        System.DateTime.Now.ToString("MM/dd/yyyy HH:mm:ss") + " ;" +
                                        DataName + " ;" +
                                        trial + " ;" +
                                        Data.envSize + " ;" + //only one size at the moment
                                        Data.locoMoInterface + " ;" + // only one interface at the moment
                                        controlModeString(Data.translationMode) + ";" +
                                        controlModeString(Data.rotationMode) + ";" +
                                        HeadsetName + ";" +
                                        Data.MSreductionMethod + ";" +
                                        Data.condition + ";" +
                                        CSAbortValue + ";" +
                                        timeElapsed.ToString("0.000") + ";" +
                                        preTrialCS + ";" +
                                        lowestNauseaScore + ";" +
                                        MinCSTime + ";" +
                                        highestNauseaScore + ";" +
                                        MaxCSTime + ";" +
                                        (highestNauseaScore - lowestNauseaScore) + ";" +
                                        vectionIntensityValue + ";" +
                                        presenceValue + ";";

            while (i <= 100)
            {
                dataCSV += PlayerPrefs.GetFloat(i.ToString()) + ";";
                i = i + 5;

            }

            dataCSV += System.Environment.NewLine;

            /* if (usFormat)
             {
                 dataCSV = dataCSV.Replace(',', '.');
                 dataCSV = dataCSV.Replace(';', ',');
             }
             else
             {
                 dataCSV = dataCSV.Replace('.', ',');
             } */

            //US-Format
            dataCSV = dataCSV.Replace(';', ',');


            File.AppendAllText(root, dataCSV);
            FileInfo fInfo = new FileInfo(root);
            fInfo.IsReadOnly = false;
        }


    }


    //Get Data for Logfile
    float RotationPlayer()
    {
        if (player.transform.eulerAngles.y > 180)
        {
            return player.transform.eulerAngles.y - 360;
        }
        else
        { return player.transform.eulerAngles.y; }
    }


    public ArrayList getData()
    {
        string t;
        if (Data.moveSpeedzones > 1)
        {
            t = "Start slow";
        }
        else
        { t = "Start fast"; }
        ArrayList data = new ArrayList();
        data.Add(Data.maxSpeed);
        data.Add(t);
        data.Add(Data.speedzoneFrequency);


        return data;
    }


    public void scoreUp(bool reset)
    {
        if (!hud.isActiveAndEnabled) return;
        hud.reset();
        if (reset) time2 += waitTime;
        //Debug.Log("Wait Time" + Convert.ToString(waitTime));
        if (currentNauseaScore < 100)
        {
            time2 += Time.fixedDeltaTime;
            if (time2 > waitTime)
            {
                currentNauseaScore += 5;
                time2 = 0;
            }
        }
        text.text = currentNauseaScore + "%";
    }

    public void scoreDown(bool reset)
    {
        if (!hud.isActiveAndEnabled) return;
        hud.reset();
        if (reset) time2 += waitTime;
        if (currentNauseaScore > 0)
        {
            time2 += Time.fixedDeltaTime;
            if (time2 > waitTime)
            {
                currentNauseaScore -= 5;
                time2 = 0;
            }

        }

        text.text = currentNauseaScore + "%";

    }

    public void submit()
    {
        StartCoroutine(CreateCSV(Data.name));
    }
    public void start()
    {
        if (!fillbar.enabled) return;
        time += Time.deltaTime;
        if (time > waitTime2)
        {
            instructions.SetActive(false);
            score.SetActive(true);
            fillbar.enabled = false;
            //playerLine.move = true;
            snow.SetActive(true);
            controls.gameStarted = true;
            //smoother plot throughout timecourse
            //timeElapsed = 0;
            startedOnce = true;
            bodySwayCondition = BodySwayMode.movement;
            linearEquation.metersTrav = 0f;
        }
    }

    public void resetStartTimer()
    {
        time = 0;
    }

    private void OnApplicationQuit()
    {
        Summary(Data.name);
    }








    string controlModeString(ControlMode c)
    {
        switch (c)
        {
            case ControlMode.passive: return "passive";
            case ControlMode.physical_active: return "physical active";
            case ControlMode.virtual_active: return "virtual active";
            default:
                return "error";


        }
    }
}
