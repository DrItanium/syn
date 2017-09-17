/**
 * @file
 * C++ wrapper class over the termbox library
 * @copyright
 * syn
 * Copyright (c) 2013-2017, Joshua Scoggins and Contributors
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef TERMBOX_H__
#define TERMBOX_H__

#include "include/termbox.h"
#include "Base.h"
#include <string.h>

namespace termbox {
    enum class SpecialKey {
#define Y(title, fix) \
        title = TB_KEY_ ## fix
#define X(title) \
        Y(title, title)
        X(F1),
        X(F2),
        X(F3),
        X(F4),
        X(F5),
        X(F6),
        X(F7),
        X(F8),
        X(F9),
        X(F10),
        X(F11),
        X(F12),
        Y(Insert,INSERT),
        Y(Delete, DELETE),
        Y(Home, HOME),
        Y(End, END),
        Y(PageUp, PGUP),
        Y(PageDown, PGDN),
        Y(ArrowUp, ARROW_UP),
        Y(ArrowDown, ARROW_DOWN),
        Y(ArrowLeft, ARROW_LEFT),
        Y(ArrowRight, ARROW_RIGHT),
        Y(MouseLeft, MOUSE_LEFT),
        Y(MouseRight, MOUSE_RIGHT),
        Y(MouseMiddle, MOUSE_MIDDLE),
        Y(MouseRelease, MOUSE_RELEASE),
        Y(MouseWheelUp, MOUSE_WHEEL_UP),
        Y(MouseWheelDown, MOUSE_WHEEL_DOWN),
        // alt modifiers
#undef X
#undef Y
    };

    using RawCell = struct tb_cell;
    using RawEvent = struct tb_event;
	using ForegroundColor = decltype(RawCell::fg);
	using BackgroundColor = decltype(RawCell::bg);
    enum class Color {
        Default = TB_DEFAULT,
        Black = TB_BLACK,
        Red = TB_RED,
        Green = TB_GREEN,
        Yellow = TB_YELLOW,
        Blue = TB_BLUE,
        Magenta = TB_MAGENTA,
        Cyan = TB_CYAN,
        White = TB_WHITE,
    };

    enum class EventType : decltype(RawEvent::type) {
        Key = TB_EVENT_KEY,
        Resize = TB_EVENT_RESIZE,
        Mouse = TB_EVENT_MOUSE,
        Error = 0,
    };

    enum class ErrorCode {
        UnsupportedTerminal = TB_EUNSUPPORTED_TERMINAL,
        FailedToOpenTTY = TB_EFAILED_TO_OPEN_TTY,
        /**
         * According to the documentation in the termbox header, this error denotes that the signal handler to deliver events failed
         */
        PipeTrapError = TB_EPIPE_TRAP_ERROR,
    };

    constexpr bool errorOccurred(int errorCode) noexcept {
		return errorCode != 0;
    }

    constexpr bool isBold(decltype(RawCell::fg) value) noexcept {
        return (value & (TB_BOLD)) != 0;
    }
    constexpr bool isUnderline(decltype(RawCell::fg) value) noexcept {
        return (value & (TB_UNDERLINE)) != 0;
    }
    constexpr bool isReverse(decltype(RawCell::fg) value) noexcept {
        return (value & (TB_REVERSE)) != 0;
    }
    constexpr Color getColor(decltype(RawCell::fg) value) noexcept {
        return static_cast<Color>(0x00FF & value);
    }
    inline int init() noexcept {
        return tb_init();
    }
    inline int init(const char* name) noexcept {
        return tb_init_file(name);
    }
    inline int init(int inout) noexcept {
        return tb_init_fd(inout);
    }

	inline int getWidth() noexcept {
		return tb_width();
	} 

	inline int getHeight() noexcept {
		return tb_height();
	}

    inline void shutdown() noexcept {
        tb_shutdown();
    }

    inline void clear() noexcept {
        tb_clear();
    }

    inline void setClearAttributes(ForegroundColor fg, BackgroundColor bg) noexcept {
        tb_set_clear_attributes(fg, bg);
    }
    /// synchronizes the internal back buffer with the terminal.
    inline void present() noexcept {
        tb_present();
    }

    constexpr auto hideCursor = TB_HIDE_CURSOR;

    inline void setCursor(int cx, int cy) noexcept {
        tb_set_cursor(cx, cy);
    }

    inline void putCell(int x, int y, const RawCell* cell) noexcept {
        tb_put_cell(x, y, cell);
    }
    inline void putCell(int x, int y, const RawCell& cell) noexcept {
        putCell(x, y, &cell);
    }

    inline void changeCell(int x, int y, decltype(RawCell::ch) ch, ForegroundColor fg, BackgroundColor bg) noexcept {
        tb_change_cell(x, y, ch, fg, bg);
    }

    inline RawCell* getCellBuffer() noexcept {
        return tb_cell_buffer();
    }

    enum InputMode : int {
        Esc = TB_INPUT_ESC,
        Alt = TB_INPUT_ALT,
        Mouse = TB_INPUT_MOUSE,
    };


    inline int getInputMode() noexcept {
        return tb_select_input_mode(TB_INPUT_CURRENT);
    }

    inline void setInputMode(InputMode mode) noexcept {
        (void)tb_select_input_mode(static_cast<int>(mode));
    }

    enum class OutputMode {
        Normal = TB_OUTPUT_NORMAL,
        Color256 = TB_OUTPUT_256,
        Color216 = TB_OUTPUT_216,
        Grayscale = TB_OUTPUT_GRAYSCALE,
    };

    inline OutputMode getOutputMode() noexcept {
        return static_cast<OutputMode>(tb_select_output_mode(TB_OUTPUT_CURRENT));
    }
    inline void setOutputMode(OutputMode mode) noexcept {
        (void)tb_select_output_mode(static_cast<int>(mode));
    }

    inline EventType peekEvent(RawEvent* event, int timeout) noexcept {
        return static_cast<EventType>(tb_peek_event(event, timeout));
    }
    inline EventType peekEvent(RawEvent& event, int timeout) noexcept {
        return peekEvent(&event, timeout);
    }
    inline EventType pollEvent(RawEvent* event) noexcept {
        return static_cast<EventType>(tb_poll_event(event));
    }
    inline EventType pollEvent(RawEvent& event) noexcept {
        return pollEvent(&event);
    }
    constexpr auto eof = TB_EOF;
    inline int utf8CharLength(char c) noexcept { return tb_utf8_char_length(c); }
    inline int utf8CharToUnicode(uint32_t* out, const char* c) noexcept {
        return tb_utf8_char_to_unicode(out, c);
    }
    inline int utf8UnicodeToChar(char* out, uint32_t c) noexcept {
        return tb_utf8_unicode_to_char(out, c);
    }

	class Event {
		public:
			using ResizePair = std::tuple<decltype(RawEvent::w), decltype(RawEvent::h)>;
			using Coordinates = std::tuple<decltype(RawEvent::x), decltype(RawEvent::y)>;
		public:
			Event(RawEvent& evt) : _evt(evt) { }
			Event(const Event& evt) : _evt(evt._evt) { }
			Event(Event&&) = delete;
			~Event() { }
			EventType getType() const noexcept { return static_cast<EventType>(_evt.type); }
			decltype(RawEvent::mod) getModifiers() const noexcept { return _evt.mod; } 
			decltype(RawEvent::key) getKey() const noexcept { return _evt.key; }
			decltype(RawEvent::ch) getCharacter() const noexcept { return _evt.ch; }
			decltype(RawEvent::w) getW() const noexcept { return _evt.w; }
			decltype(RawEvent::h) getH() const noexcept { return _evt.h; }
			decltype(RawEvent::x) getX() const noexcept { return _evt.x; }
			decltype(RawEvent::y) getY() const noexcept { return _evt.y; }
			ResizePair getResize() const noexcept { return std::make_tuple(_evt.w, _evt.h); }
			Coordinates getCoordinates() const noexcept { return std::make_tuple(_evt.x, _evt.y); }
			RawEvent& getRawEvent() const noexcept { return _evt; }
		private:
			RawEvent& _evt;
	};

	class Cell {
		public:
			Cell(RawCell& cell) : _cell(cell) { }
			Cell(Cell&&) = delete;
			Cell(const Cell& other) : Cell(other._cell) { }
			RawCell& getRawCell() const noexcept { return _cell; }
			ForegroundColor getForegroundColor() const noexcept { return _cell.fg; }
			BackgroundColor getBackgroundColor() const noexcept { return _cell.bg; }
			decltype(RawCell::ch) getCharacter() const noexcept { return _cell.ch; }
			void put(int x, int y) noexcept { putCell(x, y, &_cell); }
		private:
			RawCell& _cell;

	};

	class CellBuffer {
		public:
			CellBuffer() : _cells(termbox::getCellBuffer()) { }
			~CellBuffer() { _cells = nullptr; }
			CellBuffer(const CellBuffer& other) : _cells(other._cells) { }
			CellBuffer(CellBuffer&& other) : _cells(std::move(other._cells)) { }
			decltype(termbox::getWidth()) getWidth() const noexcept { return termbox::getWidth(); }
			decltype(termbox::getHeight()) getHeight() const noexcept { return termbox::getHeight(); }
			void present() noexcept {
				termbox::present();
				// update the back buffer after present is called!
				_cells = termbox::getCellBuffer();
			}
			void setClearAttributes(ForegroundColor fg, BackgroundColor bg) { termbox::setClearAttributes(fg, bg); }
			void clear() noexcept {
				termbox::clear();
				// update the back buffer after clear is called!
				_cells = termbox::getCellBuffer();
			}
			RawCell* getBackingBuffer() noexcept { return _cells; }
		private:
			RawCell* _cells;
	};
	constexpr auto keyEscape = TB_KEY_ESC;
	constexpr auto keyF1 = TB_KEY_F1;
	constexpr auto keyF2 = TB_KEY_F2;
	constexpr auto keyF3 = TB_KEY_F3;
	constexpr auto keyF4 = TB_KEY_F4;
	constexpr auto keyF5 = TB_KEY_F5;
	constexpr auto keyF6 = TB_KEY_F6;
	constexpr auto keyF7 = TB_KEY_F7;
	constexpr auto keyF8 = TB_KEY_F8;
	constexpr auto keyF9 = TB_KEY_F9;
	constexpr auto keyF10 = TB_KEY_F10;
	constexpr auto keyF11 = TB_KEY_F11;
	constexpr auto keyF12 = TB_KEY_F12;


	void printString(const std::string& str, int x, int y, ForegroundColor fg, BackgroundColor bg) noexcept;
} // end namespace termbox

namespace syn {
    template<>
    constexpr auto defaultErrorState<termbox::EventType> = termbox::EventType::Error;
} // end namespace syn


#endif // end TERMBOX_H__
