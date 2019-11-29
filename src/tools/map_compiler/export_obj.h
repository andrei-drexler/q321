#pragma once

const float
PI = Math::PI,
PI_2 = PI * 0.5f;

float ApproxAtan(float z)
{
    const float n1 = 0.97239411f;
    const float n2 = -0.19194795f;
    return (n1 + n2 * z * z) * z;
}

float ApproxAtan2(float y, float x)
{
    if (x != 0.0f)
    {
        if (fabsf(x) > fabsf(y))
        {
            const float z = y / x;
            if (x > 0.0)
            {
                // atan2(y,x) = atan(y/x) if x > 0
                return ApproxAtan(z);
            }
            else if (y >= 0.0)
            {
                // atan2(y,x) = atan(y/x) + PI if x < 0, y >= 0
                return ApproxAtan(z) + PI;
            }
            else
            {
                // atan2(y,x) = atan(y/x) - PI if x < 0, y < 0
                return ApproxAtan(z) - PI;
            }
        }
        else // Use property atan(y/x) = PI/2 - atan(x/y) if |y/x| > 1.
        {
            const float z = x / y;
            if (y > 0.0)
            {
                // atan2(y,x) = PI/2 - atan(x/y) if |y/x| > 1, y > 0
                return -ApproxAtan(z) + PI_2;
            }
            else
            {
                // atan2(y,x) = -PI/2 - atan(x/y) if |y/x| > 1, y < 0
                return -ApproxAtan(z) - PI_2;
            }
        }
    }
    else
    {
        if (y > 0.0f) // x = 0, y > 0
        {
            return PI_2;
        }
        else if (y < 0.0f) // x = 0, y < 0
        {
            return -PI_2;
        }
    }
    return 0.0f; // x,y = 0. Could return NaN instead.
}


bool ExportObj(const Q3::Map& map, const char* path) {
	if (map.entities.empty())
		return false;

	FILE* file = fopen(path, "wb");
	if (!file)
		return false;
	auto close_file = scope_exit { fclose(file); file = NULL; };

	size_t num_verts = 0;

	auto& world = map.entities[0];
	for (size_t brush_index=0; brush_index<world.brushes.size(); ++brush_index) {
		auto& brush = world.brushes[brush_index];
		fprintf(file, "o brush%zd\n", brush_index);

		const size_t MAX_NUM_EDGES = 256;
		BrushEdge edges[MAX_NUM_EDGES];

		size_t num_edges = EnumerateBrushEdges(brush.planes.data(), brush.planes.size(), edges, MAX_NUM_EDGES);
		
		u32 edge_mask[MAX_NUM_EDGES];
		for (size_t i=0; i<num_edges; ++i) {
			auto& e = edges[i];
			edge_mask[i] = (1<<e.first_plane) | (1<<e.second_plane);
		}

		for (size_t i=0; i<brush.planes.size(); ++i) {
			const size_t MAX_FACE_EDGES = 256;
			u8 face_edges[MAX_FACE_EDGES];
			size_t num_face_edges = 0;

			u32 face_mask = 1 << i;
			for (size_t j=0; j<num_edges; ++j)
				if (edge_mask[j] & face_mask)
					face_edges[num_face_edges++] = j;
			if (num_face_edges < 3)
				continue;

			vec3 center = 0.f;
			for (size_t j=0; j<num_face_edges; ++j) {
				auto& e = edges[face_edges[j]];
				center += e.first_point;
				center += e.second_point;
			}
			center /= float(2 * num_face_edges);
			auto& ref_edge = edges[face_edges[0]];
			auto& plane = brush.planes[i];
			vec3 x_axis = normalize((ref_edge.first_point + ref_edge.second_point) * 0.5f - center);
			vec3 y_axis = cross(x_axis, plane.xyz);

			auto get_edge_angle = [&] (u8 idx) {
				auto& edge = edges[idx];
				vec3 delta = (edge.first_point + edge.second_point) * 0.5f - center;
				return ApproxAtan2(dot(delta, y_axis), dot(delta, x_axis));
				return atan2f(dot(delta, y_axis), dot(delta, x_axis));
			};

			std::sort(std::begin(face_edges), std::begin(face_edges) + num_face_edges, [&] (u8 a, u8 b) {
				return get_edge_angle(a) > get_edge_angle(b);
			});

			for (size_t j=0; j<num_face_edges; ++j) {
				auto& edge = edges[face_edges[j]];
				vec3& p = (i == edge.first_plane) ? edge.first_point : edge.second_point;
				fprintf(file, "v %g %g %g\n", p.x, p.z, -p.y);
			}

			fprintf(file, "f");
			for (size_t j=0; j<num_face_edges; ++j)
				fprintf(file, " %zd", ++num_verts);
			fprintf(file, "\n");
		}
	}

	return true;
}
