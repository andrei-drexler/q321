#pragma once

struct alignas(u32) RectPacker {
	using Dimension				= u16;
	using Index					= u16;

	struct Rectangle {
		Dimension				min[2];
		Dimension				max[2];

		Dimension				GetWidth() const { return max[0] - min[0]; }
		Dimension				GetHeight() const { return max[1] - min[1]; }
	};
	static constexpr Rectangle*	Full = nullptr;

	void						Init(Dimension width, Dimension height);

	const Rectangle*			Add(u32 width, u32 height);
	Index						GetIndex(const Rectangle* rectangle) const { return rectangle - &m_tiles[0]; }
	const Rectangle&			GetTile(Index index) const	{ return m_tiles[index]; }
	Index						GetNumTiles() const			{ return m_numTiles; }

	Dimension					GetWidth() const			{ return m_width; }
	Dimension					GetHeight() const			{ return m_height; }

private:
	enum {
		MaxTiles				= 8192,
		MaxHeight				= 2048,
	};

	Dimension					m_width;
	Dimension					m_height;
	u32							m_numTiles;
	Array<int, MaxHeight>		m_rowSize;
	Array<Rectangle, MaxTiles>	m_tiles;
};

////////////////////////////////////////////////////////////////

FORCEINLINE void RectPacker::Init(Dimension width, Dimension height) {
	assert(height <= MaxHeight);

	MemSet(this);

	if constexpr (sizeof(m_width) == sizeof(u16)) {
		// Set both fields with a single instruction.
		// This saves a tiny bit of code since the function is forceinlined
		// and the arguments are constants.
		*(u32*) &m_width = width | (height << 16);
	} else {
		m_width = width;
		m_height = height;
	}
}

NOINLINE const RectPacker::Rectangle* RectPacker::Add(u32 width, u32 height) {
	int best_y = -1;
	int best_x = m_width;

	for (int test_y = 0, avail_height = m_height - height; test_y < avail_height; ++test_y) {
		int max_x = 0;
		for (int y = 0; y < height; ++y) {
			int current = m_rowSize[test_y + y];
			if (max_x < current)
				max_x = current;
		}
		max_x += width;

		if (max_x < best_x) {
			best_x = max_x;
			best_y = test_y;
		}
	}

	if (best_y < 0)
		Sys::Fatal(Error::Atlas);

	MemSet32(&m_rowSize[best_y], best_x, height);

	Rectangle& rect = m_tiles[m_numTiles++];

	rect.max[0] = best_x;
	best_x -= width;
	rect.min[0] = best_x;

	rect.min[1] = best_y;
	best_y += height;
	rect.max[1] = best_y;

	return &rect;
}
