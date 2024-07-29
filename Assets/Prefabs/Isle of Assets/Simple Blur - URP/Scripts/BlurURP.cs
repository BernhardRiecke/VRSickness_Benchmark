using System.Collections.Generic;
using UnityEngine;
using UnityEngine.Rendering;
using UnityEngine.Rendering.Universal;
using UnityEngine.SceneManagement;
using static SimpleBlurURP.BlurURP;

namespace SimpleBlurURP
{
    /// <summary>
    /// Blur settings for UniversalRenderPipelineAsset_Renderer
    /// </summary>
    [HelpURL("https://assetstore.unity.com/packages/slug/228567")]
    public class BlurURP : ScriptableRendererFeature
    {
        public static List<BlurURP> Instances { get; private set; } = new List<BlurURP>();

        public BlurSettings Settings = new BlurSettings();

        /// <summary>
        /// Instance parameters
        /// </summary>
        [System.Serializable]
        public class BlurSettings
        {
            public BlurSettings()
            {
                Instance = this;
            }

            public BlurSettings(BlurSettings settings)
            {
                blurPasses = settings.BlurPasses;
                downSample = settings.DownSample;
                renderPassEvent = settings.RenderPassEvent;
                blurType = settings.BlurType;
            }

            public static BlurSettings Instance { get; private set; }
            public const int BLUR_PASSES_MIN_VALUE = 1, BLUR_PASSES_MAX_VALUE = 25, DOWN_SAMPLE_MIN_VALUE = 1, DOWN_SAMPLE_MAX_VALUE = 5;

#if !UNITY_IPHONE
            [HideInInspector]
            public BlurURP BlurURP;
#endif
            public Material BlurMaterial
            {
                get
                {
                    return blurMaterial;
                }
            }

            public int BlurPasses
            {
                get
                {
                    return blurPasses;
                }
                set
                {
                    blurPasses = Mathf.Clamp(value, BLUR_PASSES_MIN_VALUE, BLUR_PASSES_MAX_VALUE);
#if !UNITY_IPHONE
                    BlurURP.Create();
#endif
                }
            }

            public int DownSample
            {
                get
                {
                    return downSample;
                }
                set
                {
                    downSample = Mathf.Clamp(value, DOWN_SAMPLE_MIN_VALUE, DOWN_SAMPLE_MAX_VALUE);
#if !UNITY_IPHONE
                    BlurURP.Create();
#endif
                }
            }

            public RenderPassEvent RenderPassEvent
            {
                get
                {
                    return renderPassEvent;
                }
                set
                {
                    renderPassEvent = value;
#if !UNITY_IPHONE
                    BlurURP.Create();
#endif
                }
            }

            public BlurTypeEnum BlurType
            {
                get
                {
                    return blurType;
                }
                set
                {
                    blurType = value;
#if !UNITY_IPHONE
                    BlurURP.Create();
#endif
                }
            }

            [Tooltip("Blur material. Use the one that is included in the asset")]
            [SerializeField]
            private Material blurMaterial;

            [Tooltip("The higher the value, the more blurry the image will be")]
            [SerializeField]
            [Range(BLUR_PASSES_MIN_VALUE, BLUR_PASSES_MAX_VALUE)]
            private int blurPasses = 3;

            [Tooltip("Rendering texture size for blurring")]
            [SerializeField]
            [Range(DOWN_SAMPLE_MIN_VALUE, DOWN_SAMPLE_MAX_VALUE)]
            private int downSample = 2;

            [Tooltip("Controls when the render pass executes")]
            [SerializeField]
            private RenderPassEvent renderPassEvent = RenderPassEvent.BeforeRenderingTransparents;

            [Tooltip("The type of blur. Lit interacts with light, but Unlit does not")]
            [SerializeField]
            private BlurTypeEnum blurType = BlurTypeEnum.Lit;

            public enum BlurTypeEnum { Lit, Unlit }
        }

        /// <summary>
        /// Blurring and transferring data to the URP renderer
        /// </summary>
        private class CustomRenderPass : ScriptableRenderPass
        {
            public Material BlurMaterial;
            public string Name, ProfilerTag;
            public int Passes, Downsample;

            private RenderTargetIdentifier[] renderTargetIdentifiers = new RenderTargetIdentifier[2];
            private RenderTargetIdentifier source;
            private int[] blurPropertyID = new int[2], salt = new int[2] { Random.Range(int.MinValue, int.MaxValue), Random.Range(int.MinValue, int.MaxValue) };

            /// <summary>
            /// Installing the source
            /// </summary>
            /// <param name="source"></param>
            public void Setup(RenderTargetIdentifier source)
            {
                this.source = source;
            }

            /// <summary>
            /// Forming a blur area considering the width and height
            /// </summary>
            /// <param name="cmd"></param>
            /// <param name="cameraTextureDescriptor"></param>
            public override void Configure(CommandBuffer cmd, RenderTextureDescriptor cameraTextureDescriptor)
            {
                for (int i = 0; i < 2; i++)
                {
                    blurPropertyID[i] = Shader.PropertyToID($"BlurID-{ salt[i] }");
                    cmd.GetTemporaryRT(blurPropertyID[i], cameraTextureDescriptor.width / Downsample, cameraTextureDescriptor.height / Downsample, 0, FilterMode.Bilinear, RenderTextureFormat.ARGB32);
                    renderTargetIdentifiers[i] = new RenderTargetIdentifier(blurPropertyID[i]);
                    ConfigureTarget(renderTargetIdentifiers[i]);
                }
            }

            /// <summary>
            /// This is where the blur algorithm happens
            /// </summary>
            /// <param name="context"></param>
            /// <param name="renderingData"></param>
            public override void Execute(ScriptableRenderContext context, ref RenderingData renderingData)
            {
                if (BlurMaterial == null)
                {
#if UNITY_EDITOR
                    Debug.LogWarning("Please specify the blur material");
#endif
                    return;
                }
                CommandBuffer cmd = CommandBufferPool.Get(ProfilerTag);
                RenderTextureDescriptor opaqueDesc = renderingData.cameraData.cameraTargetDescriptor;
                opaqueDesc.depthBufferBits = 0;
                cmd.SetGlobalFloat("_offset", 1.5f);
                cmd.Blit(source, renderTargetIdentifiers[0], BlurMaterial);
                for (int i = 0; i < Passes; i++)
                {
                    cmd.SetGlobalFloat("_offset", 1.5f + i);
                    cmd.Blit(renderTargetIdentifiers[0], renderTargetIdentifiers[1], BlurMaterial);
                    RenderTargetIdentifier rttmp = renderTargetIdentifiers[0];
                    renderTargetIdentifiers[0] = renderTargetIdentifiers[1];
                    renderTargetIdentifiers[1] = rttmp;
                }
                cmd.SetGlobalFloat("_offset", Passes - 0.5f);
                cmd.Blit(renderTargetIdentifiers[0], renderTargetIdentifiers[1], BlurMaterial);
                cmd.SetGlobalTexture(Name, renderTargetIdentifiers[1]);
                context.ExecuteCommandBuffer(cmd);
                cmd.Clear();
                CommandBufferPool.Release(cmd);
            }

            /// <summary>
            /// Cleanup any allocated data that was created during the execution of the pass
            /// </summary>
            /// <param name="cmd"></param>
            public override void FrameCleanup(CommandBuffer cmd)
            {
                for (int i = 0; i < blurPropertyID.Length; i++)
                    cmd.ReleaseTemporaryRT(blurPropertyID[i]);
            }
        }

        private CustomRenderPass blurPass;
        private GameObject blurController;
        private BlurSettings startSettings;

        /// <summary>
        /// Setting parameters when creating an instance from the inspector
        /// </summary>
        public override void Create()
        {
            if (!Instances.Contains(this))
                Instances.Add(this);
#if UNITY_EDITOR
            if (!Application.isPlaying)
                startSettings = new BlurSettings(Settings);
            else if (blurController == null && SceneManager.GetActiveScene().isLoaded)
            {
                blurController = new GameObject($"[{ Settings.BlurType.ToString() }BlurController]");
                blurController.AddComponent<BlurController>().Init(startSettings, Settings);
                DontDestroyOnLoad(blurController);
            }
#endif
            if (blurPass == null)
                blurPass = new CustomRenderPass();
            blurPass.Name = Settings.BlurType == BlurSettings.BlurTypeEnum.Lit ? "_LitBlurTexture" : "_UnlitBlurTexture";
            blurPass.ProfilerTag = "BlurURP";
            blurPass.BlurMaterial = Settings.BlurMaterial;
            blurPass.Passes = Settings.BlurPasses;
            blurPass.Downsample = Settings.DownSample;
            blurPass.renderPassEvent = Settings.RenderPassEvent;
#if !UNITY_IPHONE
            if (Settings.BlurURP == null)
                Settings.BlurURP = this;
#endif
        }

        /// <summary>
        /// Adding a rendering pass for a blur pass
        /// </summary>
        /// <param name="renderer"></param>
        /// <param name="renderingData"></param>
        public override void AddRenderPasses(ScriptableRenderer renderer, ref RenderingData renderingData)
        {
#if !UNITY_2022
            blurPass.Setup(renderer.cameraColorTarget);
#endif
            renderer.EnqueuePass(blurPass);
        }

#if UNITY_2022
        /// <summary>
        /// Contains functionality from AddRenderPasses asset of previous versions of Unity to eliminate the bug with the display of blurring
        /// </summary>
        /// <param name="renderer"></param>
        /// <param name="renderingData"></param>
        public override void SetupRenderPasses(ScriptableRenderer renderer, in RenderingData renderingData)
        {
            blurPass.Setup(renderer.cameraColorTargetHandle);
        }
#endif
    }

    [HelpURL("https://assetstore.unity.com/packages/slug/228567")]
    public class BlurController : MonoBehaviour
    {
        private int startBlurPasses, startDownSample;
        private RenderPassEvent startRenderPassEvent;
        private BlurSettings.BlurTypeEnum startBlurType;

        private BlurSettings settings;

        /// <summary>
        /// Saving initial parameter values
        /// </summary>
        public void Init(BlurSettings startSettings, BlurSettings Settings)
        {
            startBlurPasses = startSettings.BlurPasses;
            startDownSample = startSettings.DownSample;
            startRenderPassEvent = startSettings.RenderPassEvent;
            startBlurType = startSettings.BlurType;
            settings = Settings;
        }

        /// <summary>
        /// Restoring parameter values when exiting the play mode
        /// </summary>
        private void OnDestroy()
        {
            settings.BlurPasses = startBlurPasses;
            settings.DownSample = startDownSample;
            settings.RenderPassEvent = startRenderPassEvent;
            settings.BlurType = startBlurType;
        }
    }
}