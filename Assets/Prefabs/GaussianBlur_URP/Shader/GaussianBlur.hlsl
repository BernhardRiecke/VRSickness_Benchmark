/*
 * This file is subject to the terms and conditions defined in
 * file 'README.md', which is part of this source code package.
 */

/*
Texture2D tex, 
Texture2D defaultTex,  
bool useDefaultTex, 
SamplerState texSS, 
float4 uv, 
float2 screenSize, 
float iterations, 
float kernel,  
out float4 color
*/


void GaussianBlur_float(Texture2D tex, Texture2D defaultTex,  bool useDefaultTex, SamplerState texSS, float4 uv, float2 screenSize, float iterations, float kernel,  out float4 color)
{

    

    //distance from current pixel
    float d = 0.0;

    //current weight
    float cw = 0.0;

    float totalWeight = 0.0;

    float px = 1 / screenSize.x;
    float py = 1 / screenSize.y;

    float2 offset =  float2(0,0);


    color = float4(0,0,0,1);
    for (int x = -iterations; x <= iterations; x++) 
    {
        for (int y = -iterations; y <= iterations; y++) 
        {
            // color = float4(0,j,i,1);

            d = sqrt(pow(x,2) + pow(y,2));

            if (d == 0)
            {
                cw = 0;
            }
            else
            {
                cw = 1/d;
            }

            totalWeight += cw;
            offset = float2(x * px, y * py) * kernel;
			if (useDefaultTex)
			{
				color += defaultTex.Sample(texSS, float2(uv.x, uv.y) + offset) * cw;
			}
			else
			{
				color += tex.Sample(texSS, float2(uv.x, uv.y) + offset) * cw;
			}
            

        }
    }

    color /= totalWeight;
}



