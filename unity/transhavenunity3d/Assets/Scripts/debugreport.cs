using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class debugreport : MonoBehaviour
{
    public string myName;

    // Start is called before the first frame update
    void Start()
    {
        Debug.Log("I am alive!" + myName);
    }

    // Update is called once per frame
    void Update()
    {
        
    }
}
