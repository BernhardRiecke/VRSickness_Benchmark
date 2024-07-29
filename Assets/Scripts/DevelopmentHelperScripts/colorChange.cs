using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class colorChange : MonoBehaviour
{
    private void OnTriggerStay(Collider other)
    {
        if (other.tag == "Player" || other.tag == "HeightControl")
        {
            GetComponent<Renderer>().material.color = Color.green;
        }

    }
    private void OnTriggerExit(Collider other)
    {
        if (other.tag == "Player")
        {
            GetComponent<Renderer>().material.color = Color.red;
        }
        else if (other.tag == "HeightControl")
        {
            GetComponent<Renderer>().material.color = Color.blue;
        }
    }

}
