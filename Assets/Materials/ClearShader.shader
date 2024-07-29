Shader "Unlit/ClearShader"
{
	Properties
	{
		_MainTex("Texture", 2D) = "white" {}
		_Mask("Mask", 2D) = "white" {}
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

			v2f vert(appdata v)
			{
				v2f o;
				o.vertex = UnityObjectToClipPos(v.vertex);
				o.uv = v.uv;
				return o;
			}

			float4 frag(v2f i) : SV_Target
			{
				// Sample the texture and the mask
				float4 texColor = tex2D(_MainTex, i.uv);
				
				return texColor; // Display texture if the mask is transparent
				
			}
			ENDCG
		}
	}
}
