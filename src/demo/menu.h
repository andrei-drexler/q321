#pragma once

////////////////////////////////////////////////////////////////

#define DEMO_MENUS(begin, item, end)\
	begin(MainMenu)\
		/*Text,					Action,				X,		Y,		Flags*/\
		item("NEW GAME",		NewGame,			0,		120,	0)\
		item("SETUP",			Options,			0,		40,		0)\
		item("CINEMATICS",		Options,			0,		-40,	0)\
		item("EXIT",			ConfirmExitGame,	0,		-120,	0)\
	end()\
	begin(InGame)\
		item("RESUME GAME",		CloseMenu,			0,		160,	0)\
		item("SETUP",			Options,			0,		80,		0)\
		item("NEXT ARENA",		NextMap,			0,		0,		0)\
		item("LEAVE ARENA",		QuitMap,			0,		-80,	0)\
		item("EXIT GAME",		ConfirmExitGame,	0,		-160,	0)\
	end()\
	begin(ExitGameModal)\
		item("EXIT GAME?",		CloseMenu,			0,		56,		Item::Flags::Decoration)\
		item("YES",				ExitGame,			-76,	-56,	0)\
		item("/",				CloseMenu,			0,		-56,	Item::Flags::Decoration)\
		item("NO",				CloseMenu,			64,		-56,	0)\
	end()\

////////////////////////////////////////////////////////////////

namespace Demo::Menu {
	void Init();
	bool Update(float dt);
	void UpdateBannerTexture();
	void Draw();
	void ShowMainMenu();
	bool IsMainMenu(); // precondition: g_active != nullptr

	enum class Action : u8 {
		CloseMenu,
		NewGame,
		Options,
		NextMap,
		QuitMap,
		ConfirmExitGame,
		ExitGame,

		Count,
	};

	namespace Item {
		enum Flags {
			Decoration			= 1 << 0,
		};

		struct State {
			const char*			text;
			u16					flags;
			Action				action;
			vec2				pos;
		};
	};

	namespace Details {
		static constexpr u32
			ItemColor  = 0xFF'B2'00'00,
			FocusFolor = 0xFF'FF'00'00,
			DisclaimerColor = 0xFF'80'00'00;
		;

		enum {
			ItemCount = DEMO_MENUS(PP_IGNORE_ARGS, PP_INCREMENT, PP_IGNORE_ARGS),
			MenuCount = DEMO_MENUS(PP_INCREMENT, PP_IGNORE_ARGS, PP_IGNORE_ARGS),
		};

		static constexpr char ItemStringList[] =
			#define PP_ADD_ITEM_STRING(caption, action, x, y, flags) caption "\0"
			DEMO_MENUS(PP_IGNORE_ARGS, PP_ADD_ITEM_STRING, PP_IGNORE_ARGS)
			#undef PP_ADD_ITEM_STRING
		;

		static constexpr char DisclaimerText[] = {
			"demo    for mature audiences only    demo" "\0"
			"Quake III Arena(c) 1999, id Software, Inc. All Rights Reserved." "\0"
		};

		static constexpr char CreditsText[] = {
			"id software was:\0"
			" \0"
			"programming\0"
			"john carmack, robert a. duffy, jim dose'\0"
			" \0"
			"art\0"
			"adrian carmack, kevin cloud,\0"
			"kenneth scott, seneca menard, fred nilsson\0"
			" \0"
			"game designer\0"
			"graeme devine\0"
			" \0"
			"level design\0"
			"tim willits, christian antkow, jennell jaquays\0"
			" \0"
			"ceo\0"
			"todd hollenshead\0"
			" \0"
			"director of business development\0"
			"marty stratton\0"
			" \0"
			"biz assist and id mom\0"
			"donna jackson\0"
			" \0"
			"development assistance\0"
			"eric webb\0"
		};

		static constexpr u32
			NumCreditsLines = Constexpr::CountLines(Details::CreditsText),
			NumEmptyCreditsLines = Constexpr::CountEmptyLines(Details::CreditsText);

		static constexpr Action ItemActions[ItemCount] = {
			#define PP_ADD_ITEM_ACTION(caption, action, x, y, flags) Action::action,
			DEMO_MENUS(PP_IGNORE_ARGS, PP_ADD_ITEM_ACTION, PP_IGNORE_ARGS)
			#undef PP_ADD_ITEM_ACTION
		};

		static constexpr u16 ItemFlags[ItemCount] = {
			#define PP_ADD_ITEM_FLAGS(caption, action, x, y, flags) flags,
			DEMO_MENUS(PP_IGNORE_ARGS, PP_ADD_ITEM_FLAGS, PP_IGNORE_ARGS)
			#undef PP_ADD_ITEM_FLAGS
		};

		static constexpr float ItemOffsets[2][ItemCount] = {
			#define PP_ADD_ITEM_OFFSET_X(caption, action, x, y, flags) x,
			#define PP_ADD_ITEM_OFFSET_Y(caption, action, x, y, flags) y - 16.f,
			{DEMO_MENUS(PP_IGNORE_ARGS, PP_ADD_ITEM_OFFSET_X, PP_IGNORE_ARGS)},
			{DEMO_MENUS(PP_IGNORE_ARGS, PP_ADD_ITEM_OFFSET_Y, PP_IGNORE_ARGS)},
			#undef PP_ADD_ITEM_OFFSET_X
			#undef PP_ADD_ITEM_OFFSET_Y
		};

		static constexpr u8 MenuItemCounts[MenuCount] = {
			DEMO_MENUS(PP_IGNORE_ARGS, PP_INCREMENT, PP_ADD_COMMA)
		};

		Map::ID g_start_map;
	}

	struct State {
		u32						num_items;
		u32						focus;
		Item::State*			items;
		Menu::State*			prev;
	};

	Item::State					items[Details::ItemCount];
	static union {
		Menu::State				list[Details::MenuCount];
		struct {
			#define PP_ADD_MENU(name) Menu::State name;
			DEMO_MENUS(PP_ADD_MENU, PP_IGNORE_ARGS, PP_IGNORE_ARGS)
			#undef PP_ADD_MENU
		};
	};

	Menu::State*				g_active;
	i32							g_credits;

	enum class Direction : i32 {
		Back = -1,
		Forward = +1,
	};

	void						FocusFirstItem();
	void						AdvanceFocus(Direction direction = Direction::Forward);
	void						Push(Menu::State* menu);
	void						CloseAll();
	void						CloseCurrent();
}

////////////////////////////////////////////////////////////////

FORCEINLINE void Demo::Menu::FocusFirstItem() {
	g_active->focus = g_active->num_items;
	AdvanceFocus();
}

NOINLINE void Demo::Menu::AdvanceFocus(Direction direction) {
	i32 delta = i32(direction);
	u32 num_items = g_active->num_items;
	u32 focus = g_active->focus;
	const Item::State* items = g_active->items;
	u32 wrap = (num_items - 1) & (delta >> 31); // delta > 0 ? 0 : num_items - 1;
	
	do {
		focus += delta;
		// Note: underflows if direction is -1 and focus is 0,
		// producing a number that is definitely > num_items.
		// This allows us to use the same wrap-around check for both directions.
		if (focus >= num_items)
			focus = wrap;
	} while (items[focus].flags & Item::Flags::Decoration);
	
	g_active->focus = focus;
}

NOINLINE void Demo::Menu::Push(Menu::State* menu) {
	menu->prev = g_active;
	g_active = menu;
	FocusFirstItem();
}

FORCEINLINE void Demo::Menu::CloseCurrent() {
	assert(g_active);
	g_active = g_active->prev;
}

FORCEINLINE void Demo::Menu::CloseAll() {
	g_active = nullptr;
}

////////////////////////////////////////////////////////////////

FORCEINLINE void Demo::Menu::Init() {
	u32 item_index = 0;
	const char* text = Details::ItemStringList;
	do {
		Item::State& item = items[item_index];
		item.text = text;
		text = NextAfter(text);
		item.flags = Details::ItemFlags[item_index];
		item.action = Details::ItemActions[item_index];
		item.pos[0] = Details::ItemOffsets[0][item_index];
		item.pos[1] = Details::ItemOffsets[1][item_index];
	} while (++item_index < Details::ItemCount);

	u32 menu_index = 0;
	item_index = 0;
	do {
		Menu::State& menu = list[menu_index];
		u32 num_items = Details::MenuItemCounts[menu_index];
		menu.items = &items[item_index];
		menu.num_items = num_items;
		item_index += num_items;
	} while (++menu_index < Details::MenuCount);

	Details::g_start_map = Map::ID::START_MAP;
	
	Push(&MainMenu);
}

FORCEINLINE void Demo::Menu::ShowMainMenu() {
	CloseAll();
	Push(&MainMenu);
}

// precondition: g_active != nullptr
FORCEINLINE bool Demo::Menu::IsMainMenu() {
	assert(g_active);
	return !g_active->prev && !Map::IsLoaded();
}

FORCEINLINE bool Demo::Menu::Update(float dt) {
	if (g_credits) {
		if (Sys::IsAnyKeyFirstDown())
			Sys::Exit();
		return true;
	}

	if (Sys::IsKeyRepeating(Key::Escape)) {
		if (g_active) {
			if (!IsMainMenu()) // don't close main menu
				CloseCurrent();
		} else {
			Push(&InGame);
		}
	}

	if (g_active) {
		if (Sys::IsKeyRepeating(Key::Up) || Sys::IsKeyRepeating(Key::Left))
			AdvanceFocus(Direction::Back);
		else if (Sys::IsKeyRepeating(Key::Down) || Sys::IsKeyRepeating(Key::Right) || Sys::IsKeyRepeating(Key::Tab))
			AdvanceFocus(Direction::Forward);

		if (Sys::IsKeyFirstDown(Key::Enter)) {
			assert(g_active->focus < g_active->num_items);

			switch (g_active->items[g_active->focus].action) {
				case Action::CloseMenu:
					CloseCurrent();
					break;

				case Action::Options:
					break;

				case Action::NewGame:
					CloseAll();
					LoadMap(Details::g_start_map);
					break;

				case Action::NextMap:
					LoadNextMap();
					CloseAll();
					break;

				case Action::ExitGame:
					g_credits = 1;
					[[fallthrough]];

				case Action::QuitMap:
					LoadMap(Map::ID::None);
					ShowMainMenu();
					break;

				case Action::ConfirmExitGame:
					Push(&ExitGameModal);
					AdvanceFocus(); // focus 'NO'
					break;

				default:
					break;
			}
		}

		return true;
	}

	return false;
}

FORCEINLINE void Demo::Menu::UpdateBannerTexture() {
	Gfx::SetRenderTarget(Texture::menubnr);
	Gfx::SetShader(Shader::menubnr);
	Uniform::Time.x = float(g_time);
	Gfx::UpdateUniforms();
	Gfx::DrawFullScreen();
}

FORCEINLINE void Demo::Menu::Draw() {
	if (g_credits) {
		static constexpr vec2 FontScale = UI::FontScale[UI::LargeFont] * 0.75f;
		vec2 pos = {0.f, 440.f};

		for (const char* text = Details::CreditsText; *text; text = NextAfter(text)) {
			UI::Print(text, pos, FontScale, ~0, 0.5f, UI::LargeFont);
			pos.y -= *text == ' ' ? 28.f : 40.f;
		}

		UI::FlushGeometry();
		return;
	}

	if (!g_active)
		return;

	bool main_menu = IsMainMenu();
	if (main_menu)
		Gfx::SetShader(Shader::bglogo);
	else
		Gfx::SetShader(Shader::uiframe);

	if (g_active->prev) {
		// nested menu
		Uniform::Time.y = 6.f / 8.f;
		Uniform::Time.z = 3.5f / 8.f;
	} else {
		// top-level menu
		Uniform::Time.y = 7.f / 8.f;
		Uniform::Time.z = 5.f / 8.f;
	}
	Gfx::DrawFullScreen();

	if (main_menu) {
		// QUAKE III banner
		Uniform::Texture0 = Demo::Texture::menubnr;
		Gfx::SetShader(Shader::menubnr_m);
		Gfx::DrawFullScreen();

		// disclaimer text
		// TODO: decoration items with custom placement & size?

		static constexpr UI::Font Fonts[] = {
			UI::LargeFont,
			UI::SmallFont
		};
		static constexpr vec2 FontScales[] = {
			UI::FontScale[UI::LargeFont] * 0.75f,
			UI::FontScale[UI::SmallFont] * 1.5f,
		};
		vec2 pos = {0.f, -11.f/16.f * UI::VirtualHalfHeight};

		const char* text = Details::DisclaimerText;
		for (u32 line = 0; line < 2; ++line, text = NextAfter(text)) {
			UI::Print(text, pos, FontScales[line], Details::DisclaimerColor, 0.5f, Fonts[line]);
			pos.y -= 48.f;
		}
	}

	const Menu::State* menu = g_active;
	const Item::State* items = menu->items;

	const vec2& font_scale = UI::FontScale[UI::LargeFont];

	for (u32 item_index = 0; item_index < menu->num_items; ++item_index) {
		const Item::State& item = items[item_index];
		const char* text = item.text;
		bool focused = menu->focus == item_index;
		u32 color = focused ? Details::FocusFolor : Details::ItemColor;

		const vec2& pos = item.pos;
		UI::PrintShadowed(text, pos, font_scale, color, 0.5f, UI::LargeFont);

		if (focused) {
			u32 glow_alpha = u32(255.f * 2.f * abs(fract(float(g_time) * 2.f) - 0.5f)) << 24;
			color = (color & 0xFF'FF'FFu) | glow_alpha;
			UI::Print(text, pos, font_scale, color, 0.5f, UI::LargeFontBlurry);
		}
	}

	UI::FlushGeometry();
}
