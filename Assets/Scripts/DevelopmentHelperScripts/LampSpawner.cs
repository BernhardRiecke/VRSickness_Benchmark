using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class LampSpawner : MonoBehaviour
{
    public readValues rv;
    public GameObject ledLamp, ChinaLamp, cable, Player;
    float metersTraveled;
    bool ledPlaced;
    public LinearEquation le;
    bool start, roundDone = false;
    float waitTime;
    // Start is called before the first frame update
    void Start()
    {
        le.minSpeed = 0;
        le.maxSpeed = 0;
    }

    // Update is called once per frame
    void Update()
    {
        waitTime += Time.deltaTime;
        


        if (le.currentTheta <= Mathf.PI && !roundDone)
        {
            if (waitTime >= 5)
            {
                start = true;
                le.minSpeed = 5;
                le.maxSpeed = 5;
            }

            if (rv.metersTravelled - metersTraveled >=1.5 && start)
        {
            if (!ledPlaced)
            {
                if (transform.position.x > 0)
                { 
                GameObject _ledLamp = Instantiate(ledLamp, transform.localPosition + transform.right, transform.rotation);
                ledPlaced = !ledPlaced;
                _ledLamp.transform.LookAt(Player.transform);
                _ledLamp.transform.eulerAngles = new Vector3(_ledLamp.transform.eulerAngles.x + 90, _ledLamp.transform.eulerAngles.y, _ledLamp.transform.eulerAngles.z);
                _ledLamp.transform.position = new Vector3(_ledLamp.transform.position.x, _ledLamp.transform.position.y + 0.87f, _ledLamp.transform.position.z);
                }
                else
                {
                    GameObject _ledLamp = Instantiate(ledLamp, transform.localPosition - transform.right, transform.rotation);
                    ledPlaced = !ledPlaced;
                    _ledLamp.transform.LookAt(Player.transform);
                    _ledLamp.transform.eulerAngles = new Vector3(_ledLamp.transform.eulerAngles.x + 90, _ledLamp.transform.eulerAngles.y, _ledLamp.transform.eulerAngles.z);
                    _ledLamp.transform.position = new Vector3(_ledLamp.transform.position.x, _ledLamp.transform.position.y + 0.87f, _ledLamp.transform.position.z);
                }
            }
            else if (ledPlaced)
            {
                if (transform.position.x > 0)
                {
            
                    GameObject _chinaLamp = Instantiate(ChinaLamp, transform.localPosition + transform.right, transform.rotation);
                    GameObject _cable = Instantiate(cable, transform.localPosition + transform.right, new Quaternion(90, 90, 0, 0));

                    _chinaLamp.transform.LookAt(Player.transform);
                    _chinaLamp.transform.eulerAngles = new Vector3(_chinaLamp.transform.eulerAngles.x - 90, _chinaLamp.transform.eulerAngles.y + 72, _chinaLamp.transform.eulerAngles.z);
                    _chinaLamp.transform.position = new Vector3(_chinaLamp.transform.position.x + 0.12f, _chinaLamp.transform.position.y + 1, _chinaLamp.transform.position.z);



                    _cable.transform.LookAt(Player.transform);
                    _cable.transform.eulerAngles = new Vector3(_cable.transform.eulerAngles.x - 90, _cable.transform.eulerAngles.y, _cable.transform.eulerAngles.z - 90);
                    _cable.transform.position = new Vector3(_cable.transform.position.x + 0.12f, _cable.transform.position.y + 1.54f, _cable.transform.position.z);

               
                    ledPlaced = !ledPlaced;
                }
                else
                {
                    GameObject _chinaLamp = Instantiate(ChinaLamp, transform.localPosition - transform.right, transform.rotation);
                    GameObject _cable = Instantiate(cable, transform.localPosition - transform.right, new Quaternion(90, 90, 0, 0));

                    _chinaLamp.transform.LookAt(Player.transform);
                    _chinaLamp.transform.eulerAngles = new Vector3(_chinaLamp.transform.eulerAngles.x - 90, _chinaLamp.transform.eulerAngles.y + 72, _chinaLamp.transform.eulerAngles.z);
                    _chinaLamp.transform.position = new Vector3(_chinaLamp.transform.position.x + 0.12f, _chinaLamp.transform.position.y + 1, _chinaLamp.transform.position.z);



                    _cable.transform.LookAt(Player.transform);
                    _cable.transform.eulerAngles = new Vector3(_cable.transform.eulerAngles.x - 90, _cable.transform.eulerAngles.y, _cable.transform.eulerAngles.z - 90);
                    _cable.transform.position = new Vector3(_cable.transform.position.x + 0.12f, _cable.transform.position.y + 1.54f, _cable.transform.position.z);
                   
                    ledPlaced = !ledPlaced;
                }
                
            }

            metersTraveled = rv.metersTravelled;
        }
        }
      else
        {
            le.minSpeed = 10;
            le.maxSpeed = 25;
            roundDone = false;
        }

    }
}
