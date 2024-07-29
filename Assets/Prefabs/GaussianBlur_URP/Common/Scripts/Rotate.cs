using UnityEngine;
using System.Collections;

namespace GaussianBlur_LiveBlur 
{
    public class Rotate : MonoBehaviour {

        public float speed;
        private float yRotataion;
        
        // Update is called once per frame
        void FixedUpdate () 
        {
            yRotataion += speed;

            // yRotataion = Mathf.Sin(Time.time) * 10f;
            gameObject.transform.rotation = Quaternion.Euler(0f,yRotataion,0f);
        }
    }
}