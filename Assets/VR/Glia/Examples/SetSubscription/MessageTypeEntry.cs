using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

public class MessageTypeEntry : MonoBehaviour
{
    [SerializeField]
    Dropdown m_messageTypeDropdown;
    [SerializeField]
    InputField m_sender, m_id, m_subid, m_location, m_version;

    public void setDropdownItems(Dictionary<string, uint> items)
    {
        List<string> itemsList = new List<string>();
        foreach (var key in items.Keys)
        {
            itemsList.Add(key);
        }
        m_messageTypeDropdown.AddOptions(itemsList);
        m_messageTypeDropdown.RefreshShownValue();
    }

    public string getMessageTypeString()
    {
        return m_messageTypeDropdown.options[m_messageTypeDropdown.value].text;
    }
    public string getSenderString()
    {
        return m_sender.text;
    }
    public string getIdString()
    {
        return m_id.text;
    }
    public string getSubidString()
    {
        return m_subid.text;
    }
    public string getLocationString()
    {
        return m_location.text;
    }
    public string getVersionString()
    {
        return m_version.text;
    }
}
