#pragma once

struct RectPacker {
	using Dimension			= u16;
	using Index				= u16;
	static const Index Full	= -1;

	struct Rectangle {
		Dimension			min[2];
		Dimension			max[2];

		Dimension			GetWidth() const { return max[0] - min[0]; }
		Dimension			GetHeight() const { return max[1] - min[1]; }
	};

	RectPacker(Dimension width, Dimension height, Dimension expected_tiles = 64);

	Index					Add(Dimension width, Dimension height, Dimension padding = 0);
	const Rectangle&		GetTile(Index index) const	{ return m_tiles[index]; }

	Dimension				GetWidth() const			{ return m_width; }
	Dimension				GetHeight() const			{ return m_height; }

	size_t					GetUsedTexels() const		{ return m_usedTexels; }

private:
	struct Node {
		using Field			= Dimension;

		Field				flags;
		Field				data;

		static const Field	Empty = -1;
		static const Field	AxisBits = 2;
		static const Field	AxisMask = (1 << AxisBits) - 1;
		static const Field	LeafTag = AxisMask;
		static const Field	MaxIndex = Field(-1) >> AxisBits;

		Field				GetAxis() const				{ return flags & AxisMask; }
		bool				IsLeaf() const				{ return (flags & AxisMask) == LeafTag; }
		Field				GetNodeSplitPos() const		{ return data; }
		Field				GetNodeFirstChild() const	{ return flags >> AxisBits; }
		Field				IsLeafEmpty() const			{ return data == Empty; }
		Field				GetLeafImageIndex() const	{ return data; }

		static				Node MakeLeaf(Field data = Empty)								{ return { LeafTag, data }; }
		static				Node MakeInternal(bool axis, Field split_pos, Field child)		{ return { Field((child << AxisBits) | Field(axis)), split_pos }; }
	};

	bool					AddRec(unsigned node_index, Rectangle node_rect, const Dimension wanted[2]);

	Dimension				m_width;
	Dimension				m_height;
	std::vector<Node>		m_nodes;
	std::vector<Rectangle>	m_tiles;
	size_t					m_usedTexels = 0;
};

////////////////////////////////////////////////////////////////

RectPacker::RectPacker(Dimension width, Dimension height, Dimension expected_tiles) :
	m_width		(width),
	m_height	(height)
{
	m_nodes.reserve(expected_tiles * 2 + 1);
	m_nodes.push_back(Node::MakeLeaf());
	m_tiles.reserve(expected_tiles);
}

bool RectPacker::AddRec(unsigned node_index, Rectangle node_rect, const Dimension wanted[2]) {
	if (node_index >= Node::MaxIndex)
		return false;

	// Note: by value, not reference, because in the following block
	// the node vector might grow, invalidating all references...
	auto node = m_nodes[node_index];
	if (node.IsLeaf()) {
		if (!node.IsLeafEmpty())
			return false;

		if (node_rect.GetWidth() < wanted[0] || node_rect.GetHeight() < wanted[1])
			return false;

		if (node_rect.GetWidth() == wanted[0] && node_rect.GetHeight() == wanted[1]) {
			m_nodes[node_index].data = m_tiles.size();
			m_tiles.push_back(node_rect);
			return true;
		}

		auto delta_x = node_rect.GetWidth() - wanted[0];
		auto delta_y = node_rect.GetHeight() - wanted[1];
		auto axis = delta_y > delta_x;
		Node::Field split_pos = node_rect.min[axis] + wanted[axis];

		auto first_child = (Node::Field) m_nodes.size();
		// !
		m_nodes.push_back(Node::MakeLeaf());
		m_nodes.push_back(Node::MakeLeaf());

		node = m_nodes[node_index] = Node::MakeInternal(axis, split_pos, first_child);
	}

	auto axis = node.GetAxis();
	auto split_pos = node.GetNodeSplitPos();
	auto first_child = node.GetNodeFirstChild();

	auto max_pos = node_rect.max[axis];
	node_rect.max[axis] = split_pos;

	if (AddRec(first_child, node_rect, wanted))
		return true;

	node_rect.min[axis] = split_pos;
	node_rect.max[axis] = max_pos;

	return AddRec(first_child + 1, node_rect, wanted);
}

RectPacker::Index RectPacker::Add(Dimension width, Dimension height, Dimension padding) {
	if (m_tiles.size() >= std::numeric_limits<Node::Field>::max())
		return Full;

	auto max_size = std::numeric_limits<Dimension>::max();
	if (width > max_size || height > max_size)
		return Full;

	Dimension wanted[2] = { Dimension(width + padding * 2), Dimension(height + padding * 2) };
	Rectangle full{{0, 0}, {m_width, m_height}};
	if (!AddRec(0, full, wanted))
		return Full;

	auto& rect = m_tiles.back();
	m_usedTexels += rect.GetWidth() * rect.GetHeight();

	rect.min[0] += padding;
	rect.min[1] += padding;
	rect.max[0] -= padding;
	rect.max[1] -= padding;

	return Index(m_tiles.size() - 1);
}
