// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See Vblinkingled.h for the primary calling header

#include "Vblinkingled.h"
#include "Vblinkingled__Syms.h"

//==========

VL_CTOR_IMP(Vblinkingled) {
    Vblinkingled__Syms* __restrict vlSymsp = __VlSymsp = new Vblinkingled__Syms(this, name());
    Vblinkingled* const __restrict vlTOPp VL_ATTR_UNUSED = vlSymsp->TOPp;
    // Reset internal values
    
    // Reset structure values
    _ctor_var_reset();
}

void Vblinkingled::__Vconfigure(Vblinkingled__Syms* vlSymsp, bool first) {
    if (false && first) {}  // Prevent unused
    this->__VlSymsp = vlSymsp;
    if (false && this->__VlSymsp) {}  // Prevent unused
    Verilated::timeunit(-12);
    Verilated::timeprecision(-12);
}

Vblinkingled::~Vblinkingled() {
    VL_DO_CLEAR(delete __VlSymsp, __VlSymsp = NULL);
}

void Vblinkingled::_settle__TOP__2(Vblinkingled__Syms* __restrict vlSymsp) {
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vblinkingled::_settle__TOP__2\n"); );
    Vblinkingled* const __restrict vlTOPp VL_ATTR_UNUSED = vlSymsp->TOPp;
    // Body
    vlTOPp->led = (1U & ((IData)(vlTOPp->blinkingLED__DOT__counter) 
                         >> 0xfU));
}

void Vblinkingled::_eval_initial(Vblinkingled__Syms* __restrict vlSymsp) {
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vblinkingled::_eval_initial\n"); );
    Vblinkingled* const __restrict vlTOPp VL_ATTR_UNUSED = vlSymsp->TOPp;
    // Body
    vlTOPp->__Vclklast__TOP__clock = vlTOPp->clock;
}

void Vblinkingled::final() {
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vblinkingled::final\n"); );
    // Variables
    Vblinkingled__Syms* __restrict vlSymsp = this->__VlSymsp;
    Vblinkingled* const __restrict vlTOPp VL_ATTR_UNUSED = vlSymsp->TOPp;
}

void Vblinkingled::_eval_settle(Vblinkingled__Syms* __restrict vlSymsp) {
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vblinkingled::_eval_settle\n"); );
    Vblinkingled* const __restrict vlTOPp VL_ATTR_UNUSED = vlSymsp->TOPp;
    // Body
    vlTOPp->_settle__TOP__2(vlSymsp);
}

void Vblinkingled::_ctor_var_reset() {
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vblinkingled::_ctor_var_reset\n"); );
    // Body
    clock = VL_RAND_RESET_I(1);
    led = VL_RAND_RESET_I(1);
    blinkingLED__DOT__counter = VL_RAND_RESET_I(16);
}
