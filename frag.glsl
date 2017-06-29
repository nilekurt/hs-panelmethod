/*  This file is part of hs-panelmethod.

    hs-panelmethod is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    hs-panelmethod is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with hs-panelmethod.  If not, see <http://www.gnu.org/licenses/>. */


/*  Based on 2D vector field visualization by Matthias Reitinger, @mreitinger
    which itself is based on on "2D vector field visualization by Morgan McGuire,
    http://casual-effects.com", https://www.shadertoy.com/view/4s23DG */

// FRAGMENT SHADER

#version 310 es

precision mediump float;
precision mediump int;

out lowp vec4 fragColor;

layout(binding = 2) uniform Panel
{
    vec2 v1;
    vec2 v2;
    float strength;
} panels[128];
uniform int numPanels;
uniform vec2 freestream;

const float ARROW_TILE_SIZE = 32.0;
const float INVERSE_2PI = 0.1591549430918953357;

// Computes the center pixel of the tile containing pixel pos
vec2 arrowTileCenterCoord(vec2 pos)
{
    return (floor(pos / ARROW_TILE_SIZE) + 0.5) * ARROW_TILE_SIZE;
}

// Computes the signed distance from a line segment
float line(vec2 p, vec2 p1, vec2 p2)
{
    vec2 center = (p1 + p2) * 0.5;
    float len = length(p2 - p1);
    vec2 dir = (p2 - p1) / len;
    vec2 rel_p = p - center;
    float dist1 = abs(dot(rel_p, vec2(dir.y, -dir.x)));
    float dist2 = abs(dot(rel_p, dir)) - 0.5*len;
    return max(dist1, dist2);
}

// v = field sampled at arrowTileCenterCoord(p), scaled by the length
// desired in pixels for arrows
// Returns a signed distance from the arrow
float arrow(vec2 p, vec2 v)
{
    // Make everything relative to the center, which may be fractional
    p -= arrowTileCenterCoord(p);

    float mag_v = length(v), mag_p = length(p);
    
    if (mag_v > 0.0)
    {
        // Non-zero velocity case
        vec2 dir_v = v / mag_v;

        // We can't draw arrows larger than the tile radius, so clamp magnitude.
        // Enforce a minimum length to help see direction
        mag_v = clamp(mag_v, 5.0, ARROW_TILE_SIZE * 0.5);

        // Arrow tip location
        v = dir_v * mag_v;
        
        // Signed distance from shaft
        float shaft = line(p, v, -v);
        
        // Signed distance from head
        float head = min(line(p, v, 0.4*v + 0.2*vec2(-v.y, v.x)),
                     line(p, v, 0.4*v + 0.2*vec2(v.y, -v.x)));
        return min(shaft, head);
        } else {
        // Signed distance from the center point
        return mag_p;
    }
}

/////////////////////////////////////////////////////////////////////

vec2 integralCoefficient(Panel p, vec2 pos)
{
    vec2 total = vec2(0.0,0.0);

    vec2 deltaS = p.v2 - p.v1;
    float segmentCount = ceil(0.5 * length(deltaS));
    vec2 dS = deltaS / segmentCount;

    for (int i = 0; i <= int(segmentCount); ++i)
    {
        vec2 sourcePos = p.v1 + (float(i)*dS)/2.0;
        vec2 r = pos - sourcePos;
        total += r/dot(r,r);
    }
    total /= segmentCount;

    return total;
}

vec2 sourceContrib(Panel p, vec2 pos)
{
    return INVERSE_2PI * p.strength * integralCoefficient(p, pos);
}

// The vector field; use your own function or texture
vec2 field(vec2 pos)
{
    vec2 totalVelocity = freestream;
    for (int i = 0; i < numPanels; ++i)
    {
        totalVelocity += sourceContrib(panels[i],pos);
    }
    return totalVelocity;
}

void main()
{
    float arrow_dist = arrow(gl_FragCoord.xy,
                             field(arrowTileCenterCoord(gl_FragCoord.xy)) * ARROW_TILE_SIZE * 0.4);
    vec4 arrow_col = vec4(0, 0, 0, clamp(arrow_dist, 0.0, 1.0));
    vec4 field_col = vec4(field(gl_FragCoord.xy) * 0.5 + 0.5, 0.5, 1.0);
    //fragColor = mix(arrow_col, field_col, arrow_col.a);
    fragColor = (1.0,1.0,1.0,1.0);
}
