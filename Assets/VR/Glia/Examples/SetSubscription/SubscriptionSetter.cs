// (c) Copyright 2021 HP Development Company, L.P.

using System.Collections;
using HP.Omnicept.Unity;
using System.Collections.Generic;
using HP.Omnicept.Messaging.Messages;
using UnityEngine;
using UnityEngine.UI;

namespace HP.Omnicept.Unity.Examples.SetSubscription
{
    public class SubscriptionSetter : MonoBehaviour
    {
        private Dictionary<string, uint> m_subListInfo = new Dictionary<string, uint>
                                {
                                    {"None", 0},
                                    {"Camera Image", MessageTypes.ABI_MESSAGE_CAMERA_IMAGE},
                                    {"Cognitive Load", MessageTypes.ABI_MESSAGE_COGNITIVE_LOAD },
                                    {"Heart Rate", MessageTypes.ABI_MESSAGE_HEART_RATE_FRAME },
                                    {"IMU", MessageTypes.ABI_MESSAGE_IMU_FRAME},
                                    {"PPG", MessageTypes.ABI_MESSAGE_PPG_FRAME}
                                }; //add other values as needed
        public GliaBehaviour m_glia;
        public GameObject m_messageTypeUIPrefab;
        public RectTransform m_messageListContainer;
        private List<RectTransform> m_messageTypeEntries = new List<RectTransform>();

        void Start()
        {
            if (m_glia == null)
            {
                Debug.LogError("Requires Glia to be not null");
            }
            AddMessageTypeEntry();
        }

        public void SetSubscriptions()
        {
            SubscriptionList subList = new SubscriptionList();
            if (m_glia != null)
            {
                foreach (var entry in m_messageTypeEntries)
                {
                    MessageTypeEntry entryInfo = entry.GetComponent<MessageTypeEntry>();
                    uint messageType = m_subListInfo[entryInfo.getMessageTypeString()];
                    if (messageType != 0)
                    {
                        MessageVersionSemantic version = null;
                        try
                        {
                            version = new MessageVersionSemantic(entryInfo.getVersionString());
                        }
                        catch
                        {
                            Debug.Log("Version semantic " + entryInfo.getVersionString() + " is not valid");
                        }
                        if (version != null)
                        {
                            subList.Subscriptions.Add(new Subscription(messageType, entryInfo.getSenderString(), entryInfo.getIdString(), entryInfo.getSubidString(), entryInfo.getLocationString(), version));
                        }
                    }
                }
                m_glia.SetSubscriptions(subList);
            }
            else
            {
                Debug.LogError("Glia has not been set");
            }
        }

        public void AddMessageTypeEntry()
        {
            if (m_messageTypeUIPrefab != null)
            {
                GameObject messageTypeUI = Instantiate(m_messageTypeUIPrefab, m_messageListContainer.transform);
                m_messageTypeEntries.Add(messageTypeUI.GetComponent<RectTransform>());
                m_messageTypeEntries[m_messageTypeEntries.Count - 1].anchoredPosition += Vector2.up * -30 * m_messageTypeEntries.Count;

                MessageTypeEntry entry = messageTypeUI.GetComponent<MessageTypeEntry>();
                entry.setDropdownItems(m_subListInfo);
            }
            else
            {
                Debug.LogError("Requires UI component prefab set");
            }
        }

        public void RemoveMessageTypeEntry()
        {
            if (m_messageTypeEntries.Count > 0)
            {
                Destroy(m_messageTypeEntries[m_messageTypeEntries.Count - 1].gameObject);
                m_messageTypeEntries.RemoveAt(m_messageTypeEntries.Count - 1);
            }
        }
    }
}
