/*
AddTorqueOnStart.cs
Just a simple Script for the Demo
*/


using UnityEngine;
using System.Collections;

public class AddTorqueOnStart : MonoBehaviour {

	/// <summary>
    /// The randon ammount.
    /// </summary>
	public float RandonAmmount;

	/// <summary>
    /// Start this instance.
    /// </summary>
	void Start () 
	{
		
        gameObject.GetComponent<Rigidbody>().AddTorque(Random.Range(-RandonAmmount, RandonAmmount),Random.Range(-RandonAmmount, RandonAmmount),Random.Range(-RandonAmmount, RandonAmmount));
	}

}
