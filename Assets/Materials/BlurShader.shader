Shader "Unlit/BlurShader"
{
	Properties
	{
		_MainTex("Texture", 2D) = "white" {}
		_Mask("Mask", 2D) = "white" {}
		_BlurAmount("Blur Amount", float) = 1.0
	}
		SubShader
		{
			Tags { "Queue" = "Transparent" }
			Blend SrcAlpha OneMinusSrcAlpha
			Pass
			{
				CGPROGRAM
				#pragma vertex vert
				#pragma fragment frag
				#include "UnityCG.cginc"

				struct appdata
				{
					float4 vertex : POSITION;
					float2 uv : TEXCOORD0;
				};

				struct v2f
				{
					float2 uv : TEXCOORD0;
					float4 vertex : SV_POSITION;
				};

				sampler2D _MainTex;
				sampler2D _Mask;
				float _BlurAmount;

				v2f vert(appdata v)
				{
					v2f o;
					o.vertex = UnityObjectToClipPos(v.vertex);
					o.uv = v.uv;
					return o;
				}

				float4 frag(v2f i) : SV_Target
				{
					float2 uv = i.uv;
					float maskAlpha = tex2D(_Mask, uv).a;
					float4 color = float4(0, 0, 0, 0);
					float totalWeight = 0.0;

					if (maskAlpha >= 0.5)
					{
						for (float y = -4; y <= 4; y += 0.5)
						{
							for (float x = -4; x <= 4; x += 0.5)
							{
								float2 offset = float2(x, y) * 0.005;  // Reduced step size for smoother blur
								float weight = exp(-(x*x + y * y) / (2.0 * _BlurAmount * _BlurAmount));
								color += tex2D(_MainTex, uv + offset) * weight;
								totalWeight += weight;
							}
						}
						color /= totalWeight;
					}
					

					return color;
				}
				ENDCG
			}
		}
}
