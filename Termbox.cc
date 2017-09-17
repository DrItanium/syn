/**
 * @file
 * C++ wrapper class over the termbox library, impl
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

#include "Termbox.h"
#include <sstream>
#include <cstdint>
#include <cstring>

namespace termbox {
	void printString(const std::stringstream& ss, int x, int y, ForegroundColor fg, BackgroundColor bg) noexcept {
		auto str = ss.str();
		printString(str, x, y, fg, bg);
	}
	void printString(const std::string& str, int x, int y, ForegroundColor fg, BackgroundColor bg) noexcept {
		printString(str.c_str(), x, y, fg, bg);
	}
	void printString(const char* str, int x, int y, ForegroundColor fg, BackgroundColor bg) noexcept {
		// an adaption of the print_tb found in the keyboard example, it is
		// roughly the same code too :)
		while (*str) {
			uint32_t uni;
			str += utf8CharToUnicode(&uni, str);
			changeCell(x, y, uni, fg, bg);
			++x;
		}
	}
	Screen::Screen() : _width(termbox::getWidth()), _height(termbox::getHeight()), _cells(new RawCell[termbox::getWidth() * termbox::getHeight()]) { }
	Screen::~Screen() {
		if (_cells) {
			delete [] _cells;
		}
	}
	void Screen::clear() {
		memset(_cells, 0, sizeof(RawCell)*_width * _height);
	}
	void Screen::resize() {
		if (_cells) {
			delete[] _cells;
		}
		_width = getWidth();
		_height = getHeight();
		_cells = new RawCell[_width*_height];
	}

	void Screen::present() noexcept {
		termbox::clear();
		memcpy(getCellBuffer(), _cells, sizeof(RawCell)*_width*_height);
		termbox::present();
	}

	void Screen::setCell(int x, int y, uint32_t ch, ForegroundColor fg, BackgroundColor bg) noexcept {
		auto pos = _width * y + x;
		_cells[pos].ch = ch;
		_cells[pos].fg = fg;
		_cells[pos].bg = bg;
	}
	void Screen::printLine(const std::string& line, int x, int y, ForegroundColor fg, BackgroundColor bg) noexcept {
		printLine(line.c_str(), x, y, fg, bg);
	}
	void Screen::printLine(const char* line, int x, int y, ForegroundColor fg, BackgroundColor bg) noexcept {
		while (*line) {
			uint32_t uni;
			line += utf8CharToUnicode(&uni, line);
			setCell(x, y, uni, fg, bg);
			++x;
		}
	}

	void Screen::printLine(const std::stringstream& line, int x, int y, ForegroundColor fg, BackgroundColor bg) noexcept {
		auto str = line.str();
		printLine(str, x, y, fg, bg);
	}

} // end namespace termbox
