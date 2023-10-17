def self.run
  begin
    __addr__ = @addr
    __data__ = @data
    ___a__ = @_a
    ___x__ = @_x
    ___y__ = @_y
    ___pc__ = @_pc
    ___sp__ = @_sp
    __fetch__ = @fetch
    __store__ = @store
    __ram__ = @ram
    __opcode__ = @opcode
    clock = @apu.do_clock

    if clock > @clk_frame
      clock = @clk_frame
    end

    if @clk < @clk_nmi
      if clock > @clk_nmi
        clock = @clk_nmi
      end
      if @clk < @clk_irq
        if clock > @clk_irq
          clock = @clk_irq
        end
      else
        @clk_irq = 0xffffffff
        if @jammed
          return
        end
        __ram__[0x0100 + ___sp__] = ___pc__ >> 8
        ___sp__ = (___sp__ - 1) & 0xff
        __ram__[0x0100 + ___sp__] = ___pc__ & 0xff
        ___sp__ = (___sp__ - 1) & 0xff
        __ram__[0x0100 + ___sp__] = flags_pack
        ___sp__ = (___sp__ - 1) & 0xff
        @_p_i = 0x04
        @clk += 84
        addr = IRQ_VECTOR == NMI_VECTOR ? NMI_VECTOR : fetch_irq_isr_vector
        ___pc__ = (__fetch__[addr][addr] + (__fetch__[addr + 1][addr + 1] << 8))
      end
    else
      @clk_nmi = @clk_irq = 0xffffffff
      if @jammed
        return
      end
      __ram__[0x0100 + ___sp__] = ___pc__ >> 8
      ___sp__ = (___sp__ - 1) & 0xff
      __ram__[0x0100 + ___sp__] = ___pc__ & 0xff
      ___sp__ = (___sp__ - 1) & 0xff
      __ram__[0x0100 + ___sp__] = flags_pack
      ___sp__ = (___sp__ - 1) & 0xff
      @_p_i = 0x04
      @clk += 84
      addr = NMI_VECTOR == NMI_VECTOR ? NMI_VECTOR : fetch_irq_isr_vector
      ___pc__ = (__fetch__[addr][addr] + (__fetch__[addr + 1][addr + 1] << 8))
    end
    @clk_target = clock
    begin
      begin
        __opcode__ = (__fetch__[___pc__][___pc__])


        ___pc__ += 1

        case __opcode__
        when 0x00 # _brk
          data = ___pc__ + 1
          __ram__[0x0100 + ___sp__] = data >> 8
          ___sp__ = (___sp__ - 1) & 0xff
          __ram__[0x0100 + ___sp__] = data & 0xff
          ___sp__ = (___sp__ - 1) & 0xff
          data = flags_pack | 0x10
          __ram__[0x0100 + ___sp__] = data
          ___sp__ = (___sp__ - 1) & 0xff
          @_p_i = 0x04
          @clk_irq = 0xffffffff
          @clk += 84
          addr = fetch_irq_isr_vector # for inlining (__fetch__[addr][addr] + (__fetch__[addr + 1][addr + 1] << 8))
          ___pc__ = (__fetch__[addr][addr] + (__fetch__[addr + 1][addr + 1] << 8))
        when 0x01 # _ora
          addr = (__fetch__[___pc__][___pc__]) + ___x__
          ___pc__ += 1
          @clk += 60
          __addr__ = __ram__[addr & 0xff] | __ram__[(addr + 1) & 0xff] << 8
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          @_p_nz = ___a__ |= __data__
        when 0x02 # _jam
          ___pc__ = (___pc__ - 1) & 0xffff
          @clk += 24
          unless @jammed
            @jammed = true
            # interrupt reset
            @clk_nmi = 0xffffffff
            @clk_irq = 0xffffffff
            @irq_flags = 0
          end
        when 0x03 # _slo
          addr = (__fetch__[___pc__][___pc__]) + ___x__
          ___pc__ += 1
          @clk += 60
          __addr__ = __ram__[addr & 0xff] | __ram__[(addr + 1) & 0xff] << 8
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
          @_p_c = __data__ >> 7
          __data__ = __data__ << 1 & 0xff
          @_p_nz = ___a__ |= __data__
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0x04 # _nop
          ___pc__ += 1
          @clk += 3 * RP2A03_CC
        when 0x05 # _ora
          __addr__ = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          @clk += 36
          __data__ = __ram__[__addr__]
          @_p_nz = ___a__ |= __data__
        when 0x06 # _asl
          __addr__ = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          @clk += 36
          __data__ = __ram__[__addr__]
          @clk += 24
          @_p_c = __data__ >> 7
          __data__ = @_p_nz = __data__ << 1 & 0xff
          __ram__[__addr__] = __data__
        when 0x07 # _slo
          __addr__ = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          @clk += 36
          __data__ = __ram__[__addr__]
          @clk += 24
          @_p_c = __data__ >> 7
          __data__ = __data__ << 1 & 0xff
          @_p_nz = ___a__ |= __data__
          __ram__[__addr__] = __data__
        when 0x08 # _php
          @clk += 36
          data = flags_pack | 0x10
          __ram__[0x0100 + ___sp__] = data
          ___sp__ = (___sp__ - 1) & 0xff
        when 0x09 # _ora
          __data__ = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          @clk += 24
          @_p_nz = ___a__ |= __data__
        when 0x0a # _asl
          @clk += 24
          __data__ = ___a__
          @_p_c = __data__ >> 7
          __data__ = @_p_nz = __data__ << 1 & 0xff
          ___a__ = __data__
        when 0x0b # _anc
          __data__ = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          @clk += 24
          @_p_nz = ___a__ &= __data__
          @_p_c = @_p_nz >> 7
        when 0x0c # _nop
          ___pc__ += 2
          @clk += 4 * RP2A03_CC
        when 0x0d # _ora
          __addr__ = (__fetch__[___pc__][___pc__] + (__fetch__[___pc__ + 1][___pc__ + 1] << 8))
          ___pc__ += 2
          @clk += 36
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          @_p_nz = ___a__ |= __data__
        when 0x0e # _asl
          __addr__ = (__fetch__[___pc__][___pc__] + (__fetch__[___pc__ + 1][___pc__ + 1] << 8))
          ___pc__ += 2
          @clk += 36
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
          @_p_c = __data__ >> 7
          __data__ = @_p_nz = __data__ << 1 & 0xff
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0x0f # _slo
          __addr__ = (__fetch__[___pc__][___pc__] + (__fetch__[___pc__ + 1][___pc__ + 1] << 8))
          ___pc__ += 2
          @clk += 36
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
          @_p_c = __data__ >> 7
          __data__ = __data__ << 1 & 0xff
          @_p_nz = ___a__ |= __data__
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0x10 # _bpl
          if @_p_nz & 0x180 == 0
            tmp = ___pc__ + 1
            rel = (__fetch__[___pc__][___pc__])
            ___pc__ = (tmp + (rel < 128 ? rel : rel | 0xff00)) & 0xffff
            @clk += tmp[8] == ___pc__[8] ? 36 : 48
          else
            ___pc__ += 1
            @clk += 24
          end
        when 0x11 # _ora
          addr = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          indexed = __ram__[addr] + ___y__
          @clk += 48
          __addr__ = ((__ram__[(addr + 1) & 0xff] << 8) + indexed) & 0xffff
          if indexed & 0x100 != 0
            addr = (__addr__ - 0x100) & 0xffff # for inlining (__fetch__[addr][addr])
            (__fetch__[addr][addr])
            @clk += 12
          end
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          @_p_nz = ___a__ |= __data__
        when 0x12 # _jam
          ___pc__ = (___pc__ - 1) & 0xffff
          @clk += 24
          unless @jammed
            @jammed = true
            # interrupt reset
            @clk_nmi = 0xffffffff
            @clk_irq = 0xffffffff
            @irq_flags = 0
          end
        when 0x13 # _slo
          addr = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          indexed = __ram__[addr] + ___y__
          @clk += 48
          @clk += 12
          __addr__ = (__ram__[(addr + 1) & 0xff] << 8) + indexed
          addr = __addr__ - (indexed & 0x100) # for inlining (__fetch__[addr][addr])
          (__fetch__[addr][addr])
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
          @_p_c = __data__ >> 7
          __data__ = __data__ << 1 & 0xff
          @_p_nz = ___a__ |= __data__
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0x14 # _nop
          ___pc__ += 1
          @clk += 4 * RP2A03_CC
        when 0x15 # _ora
          __addr__ = (___x__ + (__fetch__[___pc__][___pc__])) & 0xff
          ___pc__ += 1
          @clk += 48
          __data__ = __ram__[__addr__]
          @_p_nz = ___a__ |= __data__
        when 0x16 # _asl
          __addr__ = (___x__ + (__fetch__[___pc__][___pc__])) & 0xff
          ___pc__ += 1
          @clk += 48
          __data__ = __ram__[__addr__]
          @clk += 24
          @_p_c = __data__ >> 7
          __data__ = @_p_nz = __data__ << 1 & 0xff
          __ram__[__addr__] = __data__
        when 0x17 # _slo
          __addr__ = (___x__ + (__fetch__[___pc__][___pc__])) & 0xff
          ___pc__ += 1
          @clk += 48
          __data__ = __ram__[__addr__]
          @clk += 24
          @_p_c = __data__ >> 7
          __data__ = __data__ << 1 & 0xff
          @_p_nz = ___a__ |= __data__
          __ram__[__addr__] = __data__
        when 0x18 # _clc
          @clk += 24
          @_p_c = 0
        when 0x19 # _ora
          addr = ___pc__ + 1
          i = ___y__ + (__fetch__[___pc__][___pc__])
          __addr__ = (((__fetch__[addr][addr]) << 8) + i) & 0xffff
          @clk += 36
          if i & 0x100 != 0
            addr = (__addr__ - 0x100) & 0xffff # for inlining (__fetch__[addr][addr])
            (__fetch__[addr][addr])
            @clk += 12
          end
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          ___pc__ += 2
          @_p_nz = ___a__ |= __data__
        when 0x1a # _nop
          ___pc__ += 0
          @clk += 2 * RP2A03_CC
        when 0x1b # _slo
          addr = ___pc__ + 1
          i = ___y__ + (__fetch__[___pc__][___pc__])
          __addr__ = (((__fetch__[addr][addr]) << 8) + i) & 0xffff
          addr = (__addr__ - (i & 0x100)) & 0xffff
          (__fetch__[addr][addr])
          @clk += 48
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
          ___pc__ += 2
          @_p_c = __data__ >> 7
          __data__ = __data__ << 1 & 0xff
          @_p_nz = ___a__ |= __data__
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0x1c # _nop
          addr = ___pc__ + 1
          i = ___x__ + (__fetch__[___pc__][___pc__])
          __addr__ = (((__fetch__[addr][addr]) << 8) + i) & 0xffff
          @clk += 36
          if i & 0x100 != 0
            addr = (__addr__ - 0x100) & 0xffff # for inlining (__fetch__[addr][addr])
            (__fetch__[addr][addr])
            @clk += 12
          end
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          ___pc__ += 2
        when 0x1d # _ora
          addr = ___pc__ + 1
          i = ___x__ + (__fetch__[___pc__][___pc__])
          __addr__ = (((__fetch__[addr][addr]) << 8) + i) & 0xffff
          @clk += 36
          if i & 0x100 != 0
            addr = (__addr__ - 0x100) & 0xffff # for inlining (__fetch__[addr][addr])
            (__fetch__[addr][addr])
            @clk += 12
          end
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          ___pc__ += 2
          @_p_nz = ___a__ |= __data__
        when 0x1e # _asl
          addr = ___pc__ + 1
          i = ___x__ + (__fetch__[___pc__][___pc__])
          __addr__ = (((__fetch__[addr][addr]) << 8) + i) & 0xffff
          addr = (__addr__ - (i & 0x100)) & 0xffff
          (__fetch__[addr][addr])
          @clk += 48
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
          ___pc__ += 2
          @_p_c = __data__ >> 7
          __data__ = @_p_nz = __data__ << 1 & 0xff
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0x1f # _slo
          addr = ___pc__ + 1
          i = ___x__ + (__fetch__[___pc__][___pc__])
          __addr__ = (((__fetch__[addr][addr]) << 8) + i) & 0xffff
          addr = (__addr__ - (i & 0x100)) & 0xffff
          (__fetch__[addr][addr])
          @clk += 48
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
          ___pc__ += 2
          @_p_c = __data__ >> 7
          __data__ = __data__ << 1 & 0xff
          @_p_nz = ___a__ |= __data__
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0x20 # _jsr
          data = ___pc__ + 1
          __ram__[0x0100 + ___sp__] = data >> 8
          ___sp__ = (___sp__ - 1) & 0xff
          __ram__[0x0100 + ___sp__] = data & 0xff
          ___sp__ = (___sp__ - 1) & 0xff
          ___pc__ = (__fetch__[___pc__][___pc__] + (__fetch__[___pc__ + 1][___pc__ + 1] << 8))
          @clk += 72
        when 0x21 # _and
          addr = (__fetch__[___pc__][___pc__]) + ___x__
          ___pc__ += 1
          @clk += 60
          __addr__ = __ram__[addr & 0xff] | __ram__[(addr + 1) & 0xff] << 8
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          @_p_nz = ___a__ &= __data__
        when 0x22 # _jam
          ___pc__ = (___pc__ - 1) & 0xffff
          @clk += 24
          unless @jammed
            @jammed = true
            # interrupt reset
            @clk_nmi = 0xffffffff
            @clk_irq = 0xffffffff
            @irq_flags = 0
          end
        when 0x23 # _rla
          addr = (__fetch__[___pc__][___pc__]) + ___x__
          ___pc__ += 1
          @clk += 60
          __addr__ = __ram__[addr & 0xff] | __ram__[(addr + 1) & 0xff] << 8
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
          c = @_p_c
          @_p_c = __data__ >> 7
          __data__ = (__data__ << 1 & 0xff) | c
          @_p_nz = ___a__ &= __data__
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0x24 # _bit
          __addr__ = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          @clk += 36
          __data__ = __ram__[__addr__]
          @_p_nz = ((__data__ & ___a__) != 0 ? 1 : 0) | ((__data__ & 0x80) << 1)
          @_p_v = __data__ & 0x40
        when 0x25 # _and
          __addr__ = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          @clk += 36
          __data__ = __ram__[__addr__]
          @_p_nz = ___a__ &= __data__
        when 0x26 # _rol
          __addr__ = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          @clk += 36
          __data__ = __ram__[__addr__]
          @clk += 24
          @_p_nz = (__data__ << 1 & 0xff) | @_p_c
          @_p_c = __data__ >> 7
          __data__ = @_p_nz
          __ram__[__addr__] = __data__
        when 0x27 # _rla
          __addr__ = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          @clk += 36
          __data__ = __ram__[__addr__]
          @clk += 24
          c = @_p_c
          @_p_c = __data__ >> 7
          __data__ = (__data__ << 1 & 0xff) | c
          @_p_nz = ___a__ &= __data__
          __ram__[__addr__] = __data__
        when 0x28 # _plp
          @clk += 48
          i = @_p_i
          flags_unpack((___sp__ = (___sp__ + 1) & 0xff; __ram__[0x0100 + ___sp__]))
          if @irq_flags != 0
            if i > @_p_i
              clk = @clk_irq = @clk + 1
              if @clk_target > clk
                @clk_target = clk
              end
            elsif i < @_p_i
              @clk_irq = 0xffffffff
              if @jammed
                return
              end
              __ram__[0x0100 + ___sp__] = ___pc__ >> 8
              ___sp__ = (___sp__ - 1) & 0xff
              __ram__[0x0100 + ___sp__] = ___pc__ & 0xff
              ___sp__ = (___sp__ - 1) & 0xff
              __ram__[0x0100 + ___sp__] = flags_pack
              ___sp__ = (___sp__ - 1) & 0xff
              @_p_i = 0x04
              @clk += 84
              addr = IRQ_VECTOR == NMI_VECTOR ? NMI_VECTOR : fetch_irq_isr_vector
              ___pc__ = (__fetch__[addr][addr] + (__fetch__[addr + 1][addr + 1] << 8))
            end
          end
        when 0x29 # _and
          __data__ = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          @clk += 24
          @_p_nz = ___a__ &= __data__
        when 0x2a # _rol
          @clk += 24
          __data__ = ___a__
          @_p_nz = (__data__ << 1 & 0xff) | @_p_c
          @_p_c = __data__ >> 7
          __data__ = @_p_nz
          ___a__ = __data__
        when 0x2b # _anc
          __data__ = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          @clk += 24
          @_p_nz = ___a__ &= __data__
          @_p_c = @_p_nz >> 7
        when 0x2c # _bit
          __addr__ = (__fetch__[___pc__][___pc__] + (__fetch__[___pc__ + 1][___pc__ + 1] << 8))
          ___pc__ += 2
          @clk += 36
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          @_p_nz = ((__data__ & ___a__) != 0 ? 1 : 0) | ((__data__ & 0x80) << 1)
          @_p_v = __data__ & 0x40
        when 0x2d # _and
          __addr__ = (__fetch__[___pc__][___pc__] + (__fetch__[___pc__ + 1][___pc__ + 1] << 8))
          ___pc__ += 2
          @clk += 36
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          @_p_nz = ___a__ &= __data__
        when 0x2e # _rol
          __addr__ = (__fetch__[___pc__][___pc__] + (__fetch__[___pc__ + 1][___pc__ + 1] << 8))
          ___pc__ += 2
          @clk += 36
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
          @_p_nz = (__data__ << 1 & 0xff) | @_p_c
          @_p_c = __data__ >> 7
          __data__ = @_p_nz
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0x2f # _rla
          __addr__ = (__fetch__[___pc__][___pc__] + (__fetch__[___pc__ + 1][___pc__ + 1] << 8))
          ___pc__ += 2
          @clk += 36
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
          c = @_p_c
          @_p_c = __data__ >> 7
          __data__ = (__data__ << 1 & 0xff) | c
          @_p_nz = ___a__ &= __data__
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0x30 # _bmi
          if @_p_nz & 0x180 != 0
            tmp = ___pc__ + 1
            rel = (__fetch__[___pc__][___pc__])
            ___pc__ = (tmp + (rel < 128 ? rel : rel | 0xff00)) & 0xffff
            @clk += tmp[8] == ___pc__[8] ? 36 : 48
          else
            ___pc__ += 1
            @clk += 24
          end
        when 0x31 # _and
          addr = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          indexed = __ram__[addr] + ___y__
          @clk += 48
          __addr__ = ((__ram__[(addr + 1) & 0xff] << 8) + indexed) & 0xffff
          if indexed & 0x100 != 0
            addr = (__addr__ - 0x100) & 0xffff # for inlining (__fetch__[addr][addr])
            (__fetch__[addr][addr])
            @clk += 12
          end
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          @_p_nz = ___a__ &= __data__
        when 0x32 # _jam
          ___pc__ = (___pc__ - 1) & 0xffff
          @clk += 24
          unless @jammed
            @jammed = true
            # interrupt reset
            @clk_nmi = 0xffffffff
            @clk_irq = 0xffffffff
            @irq_flags = 0
          end
        when 0x33 # _rla
          addr = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          indexed = __ram__[addr] + ___y__
          @clk += 48
          @clk += 12
          __addr__ = (__ram__[(addr + 1) & 0xff] << 8) + indexed
          addr = __addr__ - (indexed & 0x100) # for inlining (__fetch__[addr][addr])
          (__fetch__[addr][addr])
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
          c = @_p_c
          @_p_c = __data__ >> 7
          __data__ = (__data__ << 1 & 0xff) | c
          @_p_nz = ___a__ &= __data__
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0x34 # _nop
          ___pc__ += 1
          @clk += 4 * RP2A03_CC
        when 0x35 # _and
          __addr__ = (___x__ + (__fetch__[___pc__][___pc__])) & 0xff
          ___pc__ += 1
          @clk += 48
          __data__ = __ram__[__addr__]
          @_p_nz = ___a__ &= __data__
        when 0x36 # _rol
          __addr__ = (___x__ + (__fetch__[___pc__][___pc__])) & 0xff
          ___pc__ += 1
          @clk += 48
          __data__ = __ram__[__addr__]
          @clk += 24
          @_p_nz = (__data__ << 1 & 0xff) | @_p_c
          @_p_c = __data__ >> 7
          __data__ = @_p_nz
          __ram__[__addr__] = __data__
        when 0x37 # _rla
          __addr__ = (___x__ + (__fetch__[___pc__][___pc__])) & 0xff
          ___pc__ += 1
          @clk += 48
          __data__ = __ram__[__addr__]
          @clk += 24
          c = @_p_c
          @_p_c = __data__ >> 7
          __data__ = (__data__ << 1 & 0xff) | c
          @_p_nz = ___a__ &= __data__
          __ram__[__addr__] = __data__
        when 0x38 # _sec
          @clk += 24
          @_p_c = 1
        when 0x39 # _and
          addr = ___pc__ + 1
          i = ___y__ + (__fetch__[___pc__][___pc__])
          __addr__ = (((__fetch__[addr][addr]) << 8) + i) & 0xffff
          @clk += 36
          if i & 0x100 != 0
            addr = (__addr__ - 0x100) & 0xffff # for inlining (__fetch__[addr][addr])
            (__fetch__[addr][addr])
            @clk += 12
          end
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          ___pc__ += 2
          @_p_nz = ___a__ &= __data__
        when 0x3a # _nop
          ___pc__ += 0
          @clk += 2 * RP2A03_CC
        when 0x3b # _rla
          addr = ___pc__ + 1
          i = ___y__ + (__fetch__[___pc__][___pc__])
          __addr__ = (((__fetch__[addr][addr]) << 8) + i) & 0xffff
          addr = (__addr__ - (i & 0x100)) & 0xffff
          (__fetch__[addr][addr])
          @clk += 48
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
          ___pc__ += 2
          c = @_p_c
          @_p_c = __data__ >> 7
          __data__ = (__data__ << 1 & 0xff) | c
          @_p_nz = ___a__ &= __data__
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0x3c # _nop
          addr = ___pc__ + 1
          i = ___x__ + (__fetch__[___pc__][___pc__])
          __addr__ = (((__fetch__[addr][addr]) << 8) + i) & 0xffff
          @clk += 36
          if i & 0x100 != 0
            addr = (__addr__ - 0x100) & 0xffff # for inlining (__fetch__[addr][addr])
            (__fetch__[addr][addr])
            @clk += 12
          end
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          ___pc__ += 2
        when 0x3d # _and
          addr = ___pc__ + 1
          i = ___x__ + (__fetch__[___pc__][___pc__])
          __addr__ = (((__fetch__[addr][addr]) << 8) + i) & 0xffff
          @clk += 36
          if i & 0x100 != 0
            addr = (__addr__ - 0x100) & 0xffff # for inlining (__fetch__[addr][addr])
            (__fetch__[addr][addr])
            @clk += 12
          end
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          ___pc__ += 2
          @_p_nz = ___a__ &= __data__
        when 0x3e # _rol
          addr = ___pc__ + 1
          i = ___x__ + (__fetch__[___pc__][___pc__])
          __addr__ = (((__fetch__[addr][addr]) << 8) + i) & 0xffff
          addr = (__addr__ - (i & 0x100)) & 0xffff
          (__fetch__[addr][addr])
          @clk += 48
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
          ___pc__ += 2
          @_p_nz = (__data__ << 1 & 0xff) | @_p_c
          @_p_c = __data__ >> 7
          __data__ = @_p_nz
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0x3f # _rla
          addr = ___pc__ + 1
          i = ___x__ + (__fetch__[___pc__][___pc__])
          __addr__ = (((__fetch__[addr][addr]) << 8) + i) & 0xffff
          addr = (__addr__ - (i & 0x100)) & 0xffff
          (__fetch__[addr][addr])
          @clk += 48
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
          ___pc__ += 2
          c = @_p_c
          @_p_c = __data__ >> 7
          __data__ = (__data__ << 1 & 0xff) | c
          @_p_nz = ___a__ &= __data__
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0x40 # _rti
          @clk += 72
          packed = (___sp__ = (___sp__ + 1) & 0xff; __ram__[0x0100 + ___sp__])
          ___pc__ = ((___sp__ = (___sp__ + 1) & 0xff; __ram__[0x0100 + ___sp__]) + 256 * (___sp__ = (___sp__ + 1) & 0xff; __ram__[0x0100 + ___sp__]))
          flags_unpack(packed)
          @clk_irq = @irq_flags == 0 || @_p_i != 0 ? 0xffffffff : @clk_target = 0
        when 0x41 # _eor
          addr = (__fetch__[___pc__][___pc__]) + ___x__
          ___pc__ += 1
          @clk += 60
          __addr__ = __ram__[addr & 0xff] | __ram__[(addr + 1) & 0xff] << 8
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          @_p_nz = ___a__ ^= __data__
        when 0x42 # _jam
          ___pc__ = (___pc__ - 1) & 0xffff
          @clk += 24
          unless @jammed
            @jammed = true
            # interrupt reset
            @clk_nmi = 0xffffffff
            @clk_irq = 0xffffffff
            @irq_flags = 0
          end
        when 0x43 # _sre
          addr = (__fetch__[___pc__][___pc__]) + ___x__
          ___pc__ += 1
          @clk += 60
          __addr__ = __ram__[addr & 0xff] | __ram__[(addr + 1) & 0xff] << 8
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
          @_p_c = __data__ & 1
          __data__ >>= 1
          @_p_nz = ___a__ ^= __data__
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0x44 # _nop
          ___pc__ += 1
          @clk += 3 * RP2A03_CC
        when 0x45 # _eor
          __addr__ = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          @clk += 36
          __data__ = __ram__[__addr__]
          @_p_nz = ___a__ ^= __data__
        when 0x46 # _lsr
          __addr__ = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          @clk += 36
          __data__ = __ram__[__addr__]
          @clk += 24
          @_p_c = __data__ & 1
          __data__ = @_p_nz = __data__ >> 1
          __ram__[__addr__] = __data__
        when 0x47 # _sre
          __addr__ = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          @clk += 36
          __data__ = __ram__[__addr__]
          @clk += 24
          @_p_c = __data__ & 1
          __data__ >>= 1
          @_p_nz = ___a__ ^= __data__
          __ram__[__addr__] = __data__
        when 0x48 # _pha
          @clk += 36
          __ram__[0x0100 + ___sp__] = ___a__
          ___sp__ = (___sp__ - 1) & 0xff
        when 0x49 # _eor
          __data__ = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          @clk += 24
          @_p_nz = ___a__ ^= __data__
        when 0x4a # _lsr
          @clk += 24
          __data__ = ___a__
          @_p_c = __data__ & 1
          __data__ = @_p_nz = __data__ >> 1
          ___a__ = __data__
        when 0x4b # _asr
          __data__ = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          @clk += 24
          @_p_c = __data__ & ___a__ & 0x1
          @_p_nz = ___a__ = (__data__ & ___a__) >> 1
        when 0x4c # _jmp_a
          ___pc__ = (__fetch__[___pc__][___pc__] + (__fetch__[___pc__ + 1][___pc__ + 1] << 8))
          @clk += 36
        when 0x4d # _eor
          __addr__ = (__fetch__[___pc__][___pc__] + (__fetch__[___pc__ + 1][___pc__ + 1] << 8))
          ___pc__ += 2
          @clk += 36
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          @_p_nz = ___a__ ^= __data__
        when 0x4e # _lsr
          __addr__ = (__fetch__[___pc__][___pc__] + (__fetch__[___pc__ + 1][___pc__ + 1] << 8))
          ___pc__ += 2
          @clk += 36
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
          @_p_c = __data__ & 1
          __data__ = @_p_nz = __data__ >> 1
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0x4f # _sre
          __addr__ = (__fetch__[___pc__][___pc__] + (__fetch__[___pc__ + 1][___pc__ + 1] << 8))
          ___pc__ += 2
          @clk += 36
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
          @_p_c = __data__ & 1
          __data__ >>= 1
          @_p_nz = ___a__ ^= __data__
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0x50 # _bvc
          if @_p_v == 0
            tmp = ___pc__ + 1
            rel = (__fetch__[___pc__][___pc__])
            ___pc__ = (tmp + (rel < 128 ? rel : rel | 0xff00)) & 0xffff
            @clk += tmp[8] == ___pc__[8] ? 36 : 48
          else
            ___pc__ += 1
            @clk += 24
          end
        when 0x51 # _eor
          addr = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          indexed = __ram__[addr] + ___y__
          @clk += 48
          __addr__ = ((__ram__[(addr + 1) & 0xff] << 8) + indexed) & 0xffff
          if indexed & 0x100 != 0
            addr = (__addr__ - 0x100) & 0xffff # for inlining (__fetch__[addr][addr])
            (__fetch__[addr][addr])
            @clk += 12
          end
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          @_p_nz = ___a__ ^= __data__
        when 0x52 # _jam
          ___pc__ = (___pc__ - 1) & 0xffff
          @clk += 24
          unless @jammed
            @jammed = true
            # interrupt reset
            @clk_nmi = 0xffffffff
            @clk_irq = 0xffffffff
            @irq_flags = 0
          end
        when 0x53 # _sre
          addr = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          indexed = __ram__[addr] + ___y__
          @clk += 48
          @clk += 12
          __addr__ = (__ram__[(addr + 1) & 0xff] << 8) + indexed
          addr = __addr__ - (indexed & 0x100) # for inlining (__fetch__[addr][addr])
          (__fetch__[addr][addr])
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
          @_p_c = __data__ & 1
          __data__ >>= 1
          @_p_nz = ___a__ ^= __data__
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0x54 # _nop
          ___pc__ += 1
          @clk += 4 * RP2A03_CC
        when 0x55 # _eor
          __addr__ = (___x__ + (__fetch__[___pc__][___pc__])) & 0xff
          ___pc__ += 1
          @clk += 48
          __data__ = __ram__[__addr__]
          @_p_nz = ___a__ ^= __data__
        when 0x56 # _lsr
          __addr__ = (___x__ + (__fetch__[___pc__][___pc__])) & 0xff
          ___pc__ += 1
          @clk += 48
          __data__ = __ram__[__addr__]
          @clk += 24
          @_p_c = __data__ & 1
          __data__ = @_p_nz = __data__ >> 1
          __ram__[__addr__] = __data__
        when 0x57 # _sre
          __addr__ = (___x__ + (__fetch__[___pc__][___pc__])) & 0xff
          ___pc__ += 1
          @clk += 48
          __data__ = __ram__[__addr__]
          @clk += 24
          @_p_c = __data__ & 1
          __data__ >>= 1
          @_p_nz = ___a__ ^= __data__
          __ram__[__addr__] = __data__
        when 0x58 # _cli
          @clk += 24
          if @_p_i != 0
            @_p_i = 0
            if @irq_flags != 0
              clk = @clk_irq = @clk + 1
              if @clk_target > clk
                @clk_target = clk
              end
            end
          end
        when 0x59 # _eor
          addr = ___pc__ + 1
          i = ___y__ + (__fetch__[___pc__][___pc__])
          __addr__ = (((__fetch__[addr][addr]) << 8) + i) & 0xffff
          @clk += 36
          if i & 0x100 != 0
            addr = (__addr__ - 0x100) & 0xffff # for inlining (__fetch__[addr][addr])
            (__fetch__[addr][addr])
            @clk += 12
          end
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          ___pc__ += 2
          @_p_nz = ___a__ ^= __data__
        when 0x5a # _nop
          ___pc__ += 0
          @clk += 2 * RP2A03_CC
        when 0x5b # _sre
          addr = ___pc__ + 1
          i = ___y__ + (__fetch__[___pc__][___pc__])
          __addr__ = (((__fetch__[addr][addr]) << 8) + i) & 0xffff
          addr = (__addr__ - (i & 0x100)) & 0xffff
          (__fetch__[addr][addr])
          @clk += 48
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
          ___pc__ += 2
          @_p_c = __data__ & 1
          __data__ >>= 1
          @_p_nz = ___a__ ^= __data__
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0x5c # _nop
          addr = ___pc__ + 1
          i = ___x__ + (__fetch__[___pc__][___pc__])
          __addr__ = (((__fetch__[addr][addr]) << 8) + i) & 0xffff
          @clk += 36
          if i & 0x100 != 0
            addr = (__addr__ - 0x100) & 0xffff # for inlining (__fetch__[addr][addr])
            (__fetch__[addr][addr])
            @clk += 12
          end
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          ___pc__ += 2
        when 0x5d # _eor
          addr = ___pc__ + 1
          i = ___x__ + (__fetch__[___pc__][___pc__])
          __addr__ = (((__fetch__[addr][addr]) << 8) + i) & 0xffff
          @clk += 36
          if i & 0x100 != 0
            addr = (__addr__ - 0x100) & 0xffff # for inlining (__fetch__[addr][addr])
            (__fetch__[addr][addr])
            @clk += 12
          end
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          ___pc__ += 2
          @_p_nz = ___a__ ^= __data__
        when 0x5e # _lsr
          addr = ___pc__ + 1
          i = ___x__ + (__fetch__[___pc__][___pc__])
          __addr__ = (((__fetch__[addr][addr]) << 8) + i) & 0xffff
          addr = (__addr__ - (i & 0x100)) & 0xffff
          (__fetch__[addr][addr])
          @clk += 48
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
          ___pc__ += 2
          @_p_c = __data__ & 1
          __data__ = @_p_nz = __data__ >> 1
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0x5f # _sre
          addr = ___pc__ + 1
          i = ___x__ + (__fetch__[___pc__][___pc__])
          __addr__ = (((__fetch__[addr][addr]) << 8) + i) & 0xffff
          addr = (__addr__ - (i & 0x100)) & 0xffff
          (__fetch__[addr][addr])
          @clk += 48
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
          ___pc__ += 2
          @_p_c = __data__ & 1
          __data__ >>= 1
          @_p_nz = ___a__ ^= __data__
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0x60 # _rts
          ___pc__ = (((___sp__ = (___sp__ + 1) & 0xff; __ram__[0x0100 + ___sp__]) + 256 * (___sp__ = (___sp__ + 1) & 0xff; __ram__[0x0100 + ___sp__])) + 1) & 0xffff
          @clk += 72
        when 0x61 # _adc
          addr = (__fetch__[___pc__][___pc__]) + ___x__
          ___pc__ += 1
          @clk += 60
          __addr__ = __ram__[addr & 0xff] | __ram__[(addr + 1) & 0xff] << 8
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          tmp = ___a__ + __data__ + @_p_c
          @_p_v = ~(___a__ ^ __data__) & (___a__ ^ tmp) & 0x80
          @_p_nz = ___a__ = tmp & 0xff
          @_p_c = tmp[8]
        when 0x62 # _jam
          ___pc__ = (___pc__ - 1) & 0xffff
          @clk += 24
          unless @jammed
            @jammed = true
            # interrupt reset
            @clk_nmi = 0xffffffff
            @clk_irq = 0xffffffff
            @irq_flags = 0
          end
        when 0x63 # _rra
          addr = (__fetch__[___pc__][___pc__]) + ___x__
          ___pc__ += 1
          @clk += 60
          __addr__ = __ram__[addr & 0xff] | __ram__[(addr + 1) & 0xff] << 8
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
          c = @_p_c << 7
          @_p_c = __data__ & 1
          __data__ = (__data__ >> 1) | c
          tmp = ___a__ + __data__ + @_p_c
          @_p_v = ~(___a__ ^ __data__) & (___a__ ^ tmp) & 0x80
          @_p_nz = ___a__ = tmp & 0xff
          @_p_c = tmp[8]
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0x64 # _nop
          ___pc__ += 1
          @clk += 3 * RP2A03_CC
        when 0x65 # _adc
          __addr__ = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          @clk += 36
          __data__ = __ram__[__addr__]
          tmp = ___a__ + __data__ + @_p_c
          @_p_v = ~(___a__ ^ __data__) & (___a__ ^ tmp) & 0x80
          @_p_nz = ___a__ = tmp & 0xff
          @_p_c = tmp[8]
        when 0x66 # _ror
          __addr__ = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          @clk += 36
          __data__ = __ram__[__addr__]
          @clk += 24
          @_p_nz = (__data__ >> 1) | (@_p_c << 7)
          @_p_c = __data__ & 1
          __data__ = @_p_nz
          __ram__[__addr__] = __data__
        when 0x67 # _rra
          __addr__ = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          @clk += 36
          __data__ = __ram__[__addr__]
          @clk += 24
          c = @_p_c << 7
          @_p_c = __data__ & 1
          __data__ = (__data__ >> 1) | c
          tmp = ___a__ + __data__ + @_p_c
          @_p_v = ~(___a__ ^ __data__) & (___a__ ^ tmp) & 0x80
          @_p_nz = ___a__ = tmp & 0xff
          @_p_c = tmp[8]
          __ram__[__addr__] = __data__
        when 0x68 # _pla
          @clk += 48
          @_p_nz = ___a__ = (___sp__ = (___sp__ + 1) & 0xff; __ram__[0x0100 + ___sp__])
        when 0x69 # _adc
          __data__ = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          @clk += 24
          tmp = ___a__ + __data__ + @_p_c
          @_p_v = ~(___a__ ^ __data__) & (___a__ ^ tmp) & 0x80
          @_p_nz = ___a__ = tmp & 0xff
          @_p_c = tmp[8]
        when 0x6a # _ror
          @clk += 24
          __data__ = ___a__
          @_p_nz = (__data__ >> 1) | (@_p_c << 7)
          @_p_c = __data__ & 1
          __data__ = @_p_nz
          ___a__ = __data__
        when 0x6b # _arr
          __data__ = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          @clk += 24
          ___a__ = ((__data__ & ___a__) >> 1) | (@_p_c << 7)
          @_p_nz = ___a__
          @_p_c = ___a__[6]
          @_p_v = ___a__[6] ^ ___a__[5]
        when 0x6c # _jmp_i
          pos = (__fetch__[___pc__][___pc__] + (__fetch__[___pc__ + 1][___pc__ + 1] << 8))
          low = (__fetch__[pos][pos])
          pos = (pos & 0xff00) | ((pos + 1) & 0x00ff)
          high = (__fetch__[pos][pos])
          ___pc__ = high * 256 + low
          @clk += 60
        when 0x6d # _adc
          __addr__ = (__fetch__[___pc__][___pc__] + (__fetch__[___pc__ + 1][___pc__ + 1] << 8))
          ___pc__ += 2
          @clk += 36
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          tmp = ___a__ + __data__ + @_p_c
          @_p_v = ~(___a__ ^ __data__) & (___a__ ^ tmp) & 0x80
          @_p_nz = ___a__ = tmp & 0xff
          @_p_c = tmp[8]
        when 0x6e # _ror
          __addr__ = (__fetch__[___pc__][___pc__] + (__fetch__[___pc__ + 1][___pc__ + 1] << 8))
          ___pc__ += 2
          @clk += 36
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
          @_p_nz = (__data__ >> 1) | (@_p_c << 7)
          @_p_c = __data__ & 1
          __data__ = @_p_nz
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0x6f # _rra
          __addr__ = (__fetch__[___pc__][___pc__] + (__fetch__[___pc__ + 1][___pc__ + 1] << 8))
          ___pc__ += 2
          @clk += 36
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
          c = @_p_c << 7
          @_p_c = __data__ & 1
          __data__ = (__data__ >> 1) | c
          tmp = ___a__ + __data__ + @_p_c
          @_p_v = ~(___a__ ^ __data__) & (___a__ ^ tmp) & 0x80
          @_p_nz = ___a__ = tmp & 0xff
          @_p_c = tmp[8]
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0x70 # _bvs
          if @_p_v != 0
            tmp = ___pc__ + 1
            rel = (__fetch__[___pc__][___pc__])
            ___pc__ = (tmp + (rel < 128 ? rel : rel | 0xff00)) & 0xffff
            @clk += tmp[8] == ___pc__[8] ? 36 : 48
          else
            ___pc__ += 1
            @clk += 24
          end
        when 0x71 # _adc
          addr = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          indexed = __ram__[addr] + ___y__
          @clk += 48
          __addr__ = ((__ram__[(addr + 1) & 0xff] << 8) + indexed) & 0xffff
          if indexed & 0x100 != 0
            addr = (__addr__ - 0x100) & 0xffff # for inlining (__fetch__[addr][addr])
            (__fetch__[addr][addr])
            @clk += 12
          end
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          tmp = ___a__ + __data__ + @_p_c
          @_p_v = ~(___a__ ^ __data__) & (___a__ ^ tmp) & 0x80
          @_p_nz = ___a__ = tmp & 0xff
          @_p_c = tmp[8]
        when 0x72 # _jam
          ___pc__ = (___pc__ - 1) & 0xffff
          @clk += 24
          unless @jammed
            @jammed = true
            # interrupt reset
            @clk_nmi = 0xffffffff
            @clk_irq = 0xffffffff
            @irq_flags = 0
          end
        when 0x73 # _rra
          addr = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          indexed = __ram__[addr] + ___y__
          @clk += 48
          @clk += 12
          __addr__ = (__ram__[(addr + 1) & 0xff] << 8) + indexed
          addr = __addr__ - (indexed & 0x100) # for inlining (__fetch__[addr][addr])
          (__fetch__[addr][addr])
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
          c = @_p_c << 7
          @_p_c = __data__ & 1
          __data__ = (__data__ >> 1) | c
          tmp = ___a__ + __data__ + @_p_c
          @_p_v = ~(___a__ ^ __data__) & (___a__ ^ tmp) & 0x80
          @_p_nz = ___a__ = tmp & 0xff
          @_p_c = tmp[8]
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0x74 # _nop
          ___pc__ += 1
          @clk += 4 * RP2A03_CC
        when 0x75 # _adc
          __addr__ = (___x__ + (__fetch__[___pc__][___pc__])) & 0xff
          ___pc__ += 1
          @clk += 48
          __data__ = __ram__[__addr__]
          tmp = ___a__ + __data__ + @_p_c
          @_p_v = ~(___a__ ^ __data__) & (___a__ ^ tmp) & 0x80
          @_p_nz = ___a__ = tmp & 0xff
          @_p_c = tmp[8]
        when 0x76 # _ror
          __addr__ = (___x__ + (__fetch__[___pc__][___pc__])) & 0xff
          ___pc__ += 1
          @clk += 48
          __data__ = __ram__[__addr__]
          @clk += 24
          @_p_nz = (__data__ >> 1) | (@_p_c << 7)
          @_p_c = __data__ & 1
          __data__ = @_p_nz
          __ram__[__addr__] = __data__
        when 0x77 # _rra
          __addr__ = (___x__ + (__fetch__[___pc__][___pc__])) & 0xff
          ___pc__ += 1
          @clk += 48
          __data__ = __ram__[__addr__]
          @clk += 24
          c = @_p_c << 7
          @_p_c = __data__ & 1
          __data__ = (__data__ >> 1) | c
          tmp = ___a__ + __data__ + @_p_c
          @_p_v = ~(___a__ ^ __data__) & (___a__ ^ tmp) & 0x80
          @_p_nz = ___a__ = tmp & 0xff
          @_p_c = tmp[8]
          __ram__[__addr__] = __data__
        when 0x78 # _sei
          @clk += 24
          if @_p_i == 0
            @_p_i = 0x04
            @clk_irq = 0xffffffff
            if @irq_flags != 0
              if @jammed
                return
              end
              __ram__[0x0100 + ___sp__] = ___pc__ >> 8
              ___sp__ = (___sp__ - 1) & 0xff
              __ram__[0x0100 + ___sp__] = ___pc__ & 0xff
              ___sp__ = (___sp__ - 1) & 0xff
              __ram__[0x0100 + ___sp__] = flags_pack
              ___sp__ = (___sp__ - 1) & 0xff
              @_p_i = 0x04
              @clk += 84
              addr = IRQ_VECTOR == NMI_VECTOR ? NMI_VECTOR : fetch_irq_isr_vector
              ___pc__ = (__fetch__[addr][addr] + (__fetch__[addr + 1][addr + 1] << 8))
            end
          end
        when 0x79 # _adc
          addr = ___pc__ + 1
          i = ___y__ + (__fetch__[___pc__][___pc__])
          __addr__ = (((__fetch__[addr][addr]) << 8) + i) & 0xffff
          @clk += 36
          if i & 0x100 != 0
            addr = (__addr__ - 0x100) & 0xffff # for inlining (__fetch__[addr][addr])
            (__fetch__[addr][addr])
            @clk += 12
          end
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          ___pc__ += 2
          tmp = ___a__ + __data__ + @_p_c
          @_p_v = ~(___a__ ^ __data__) & (___a__ ^ tmp) & 0x80
          @_p_nz = ___a__ = tmp & 0xff
          @_p_c = tmp[8]
        when 0x7a # _nop
          ___pc__ += 0
          @clk += 2 * RP2A03_CC
        when 0x7b # _rra
          addr = ___pc__ + 1
          i = ___y__ + (__fetch__[___pc__][___pc__])
          __addr__ = (((__fetch__[addr][addr]) << 8) + i) & 0xffff
          addr = (__addr__ - (i & 0x100)) & 0xffff
          (__fetch__[addr][addr])
          @clk += 48
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
          ___pc__ += 2
          c = @_p_c << 7
          @_p_c = __data__ & 1
          __data__ = (__data__ >> 1) | c
          tmp = ___a__ + __data__ + @_p_c
          @_p_v = ~(___a__ ^ __data__) & (___a__ ^ tmp) & 0x80
          @_p_nz = ___a__ = tmp & 0xff
          @_p_c = tmp[8]
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0x7c # _nop
          addr = ___pc__ + 1
          i = ___x__ + (__fetch__[___pc__][___pc__])
          __addr__ = (((__fetch__[addr][addr]) << 8) + i) & 0xffff
          @clk += 36
          if i & 0x100 != 0
            addr = (__addr__ - 0x100) & 0xffff # for inlining (__fetch__[addr][addr])
            (__fetch__[addr][addr])
            @clk += 12
          end
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          ___pc__ += 2
        when 0x7d # _adc
          addr = ___pc__ + 1
          i = ___x__ + (__fetch__[___pc__][___pc__])
          __addr__ = (((__fetch__[addr][addr]) << 8) + i) & 0xffff
          @clk += 36
          if i & 0x100 != 0
            addr = (__addr__ - 0x100) & 0xffff # for inlining (__fetch__[addr][addr])
            (__fetch__[addr][addr])
            @clk += 12
          end
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          ___pc__ += 2
          tmp = ___a__ + __data__ + @_p_c
          @_p_v = ~(___a__ ^ __data__) & (___a__ ^ tmp) & 0x80
          @_p_nz = ___a__ = tmp & 0xff
          @_p_c = tmp[8]
        when 0x7e # _ror
          addr = ___pc__ + 1
          i = ___x__ + (__fetch__[___pc__][___pc__])
          __addr__ = (((__fetch__[addr][addr]) << 8) + i) & 0xffff
          addr = (__addr__ - (i & 0x100)) & 0xffff
          (__fetch__[addr][addr])
          @clk += 48
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
          ___pc__ += 2
          @_p_nz = (__data__ >> 1) | (@_p_c << 7)
          @_p_c = __data__ & 1
          __data__ = @_p_nz
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0x7f # _rra
          addr = ___pc__ + 1
          i = ___x__ + (__fetch__[___pc__][___pc__])
          __addr__ = (((__fetch__[addr][addr]) << 8) + i) & 0xffff
          addr = (__addr__ - (i & 0x100)) & 0xffff
          (__fetch__[addr][addr])
          @clk += 48
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
          ___pc__ += 2
          c = @_p_c << 7
          @_p_c = __data__ & 1
          __data__ = (__data__ >> 1) | c
          tmp = ___a__ + __data__ + @_p_c
          @_p_v = ~(___a__ ^ __data__) & (___a__ ^ tmp) & 0x80
          @_p_nz = ___a__ = tmp & 0xff
          @_p_c = tmp[8]
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0x80 # _nop
          ___pc__ += 1
          @clk += 2 * RP2A03_CC
        when 0x81 # _sta
          addr = (__fetch__[___pc__][___pc__]) + ___x__
          ___pc__ += 1
          @clk += 60
          __addr__ = __ram__[addr & 0xff] | __ram__[(addr + 1) & 0xff] << 8
          __data__ = ___a__
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0x82 # _nop
          ___pc__ += 1
          @clk += 2 * RP2A03_CC
        when 0x83 # _sax
          addr = (__fetch__[___pc__][___pc__]) + ___x__
          ___pc__ += 1
          @clk += 60
          __addr__ = __ram__[addr & 0xff] | __ram__[(addr + 1) & 0xff] << 8
          __data__ = ___a__ & ___x__
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0x84 # _sty
          __addr__ = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          @clk += 36
          __data__ = ___y__
          __ram__[__addr__] = __data__
        when 0x85 # _sta
          __addr__ = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          @clk += 36
          __data__ = ___a__
          __ram__[__addr__] = __data__
        when 0x86 # _stx
          __addr__ = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          @clk += 36
          __data__ = ___x__
          __ram__[__addr__] = __data__
        when 0x87 # _sax
          __addr__ = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          @clk += 36
          __data__ = ___a__ & ___x__
          __ram__[__addr__] = __data__
        when 0x88 # _dey
          @clk += 24
          __data__ = @_p_nz = ___y__ = (___y__ - 1) & 0xff
        when 0x89 # _nop
          ___pc__ += 1
          @clk += 2 * RP2A03_CC
        when 0x8a # _txa
          @clk += 24
          @_p_nz = ___a__ = ___x__
        when 0x8b # _ane
          __data__ = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          @clk += 24
          ___a__ = (___a__ | 0xee) & ___x__ & __data__
          @_p_nz = ___a__
        when 0x8c # _sty
          __addr__ = (__fetch__[___pc__][___pc__] + (__fetch__[___pc__ + 1][___pc__ + 1] << 8))
          ___pc__ += 2
          @clk += 36
          __data__ = ___y__
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0x8d # _sta
          __addr__ = (__fetch__[___pc__][___pc__] + (__fetch__[___pc__ + 1][___pc__ + 1] << 8))
          ___pc__ += 2
          @clk += 36
          __data__ = ___a__
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0x8e # _stx
          __addr__ = (__fetch__[___pc__][___pc__] + (__fetch__[___pc__ + 1][___pc__ + 1] << 8))
          ___pc__ += 2
          @clk += 36
          __data__ = ___x__
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0x8f # _sax
          __addr__ = (__fetch__[___pc__][___pc__] + (__fetch__[___pc__ + 1][___pc__ + 1] << 8))
          ___pc__ += 2
          @clk += 36
          __data__ = ___a__ & ___x__
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0x90 # _bcc
          if @_p_c == 0
            tmp = ___pc__ + 1
            rel = (__fetch__[___pc__][___pc__])
            ___pc__ = (tmp + (rel < 128 ? rel : rel | 0xff00)) & 0xffff
            @clk += tmp[8] == ___pc__[8] ? 36 : 48
          else
            ___pc__ += 1
            @clk += 24
          end
        when 0x91 # _sta
          addr = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          indexed = __ram__[addr] + ___y__
          @clk += 48
          @clk += 12
          __addr__ = (__ram__[(addr + 1) & 0xff] << 8) + indexed
          addr = __addr__ - (indexed & 0x100) # for inlining (__fetch__[addr][addr])
          (__fetch__[addr][addr])
          __data__ = ___a__
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0x92 # _jam
          ___pc__ = (___pc__ - 1) & 0xffff
          @clk += 24
          unless @jammed
            @jammed = true
            # interrupt reset
            @clk_nmi = 0xffffffff
            @clk_irq = 0xffffffff
            @irq_flags = 0
          end
        when 0x93 # _sha
          addr = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          indexed = __ram__[addr] + ___y__
          @clk += 48
          @clk += 12
          __addr__ = (__ram__[(addr + 1) & 0xff] << 8) + indexed
          addr = __addr__ - (indexed & 0x100) # for inlining (__fetch__[addr][addr])
          (__fetch__[addr][addr])
          __data__ = ___a__ & ___x__ & ((__addr__ >> 8) + 1)
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0x94 # _sty
          __addr__ = (___x__ + (__fetch__[___pc__][___pc__])) & 0xff
          ___pc__ += 1
          @clk += 48
          __data__ = ___y__
          __ram__[__addr__] = __data__
        when 0x95 # _sta
          __addr__ = (___x__ + (__fetch__[___pc__][___pc__])) & 0xff
          ___pc__ += 1
          @clk += 48
          __data__ = ___a__
          __ram__[__addr__] = __data__
        when 0x96 # _stx
          __addr__ = (___y__ + (__fetch__[___pc__][___pc__])) & 0xff
          ___pc__ += 1
          @clk += 48
          __data__ = ___x__
          __ram__[__addr__] = __data__
        when 0x97 # _sax
          __addr__ = (___y__ + (__fetch__[___pc__][___pc__])) & 0xff
          ___pc__ += 1
          @clk += 48
          __data__ = ___a__ & ___x__
          __ram__[__addr__] = __data__
        when 0x98 # _tya
          @clk += 24
          @_p_nz = ___a__ = ___y__
        when 0x99 # _sta
          addr = ___pc__ + 1
          i = ___y__ + (__fetch__[___pc__][___pc__])
          __addr__ = (((__fetch__[addr][addr]) << 8) + i) & 0xffff
          addr = (__addr__ - (i & 0x100)) & 0xffff
          (__fetch__[addr][addr])
          @clk += 48
          ___pc__ += 2
          __data__ = ___a__
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0x9a # _txs
          @clk += 24
          ___sp__ = ___x__
        when 0x9b # _shs
          addr = ___pc__ + 1
          i = ___y__ + (__fetch__[___pc__][___pc__])
          __addr__ = (((__fetch__[addr][addr]) << 8) + i) & 0xffff
          addr = (__addr__ - (i & 0x100)) & 0xffff
          (__fetch__[addr][addr])
          @clk += 48
          ___pc__ += 2
          ___sp__ = ___a__ & ___x__
          __data__ = ___sp__ & ((__addr__ >> 8) + 1)
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0x9c # _shy
          addr = ___pc__ + 1
          i = ___x__ + (__fetch__[___pc__][___pc__])
          __addr__ = (((__fetch__[addr][addr]) << 8) + i) & 0xffff
          addr = (__addr__ - (i & 0x100)) & 0xffff
          (__fetch__[addr][addr])
          @clk += 48
          ___pc__ += 2
          __data__ = ___y__ & ((__addr__ >> 8) + 1)
          __addr__ = (__data__ << 8) | (__addr__ & 0xff)
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0x9d # _sta
          addr = ___pc__ + 1
          i = ___x__ + (__fetch__[___pc__][___pc__])
          __addr__ = (((__fetch__[addr][addr]) << 8) + i) & 0xffff
          addr = (__addr__ - (i & 0x100)) & 0xffff
          (__fetch__[addr][addr])
          @clk += 48
          ___pc__ += 2
          __data__ = ___a__
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0x9e # _shx
          addr = ___pc__ + 1
          i = ___y__ + (__fetch__[___pc__][___pc__])
          __addr__ = (((__fetch__[addr][addr]) << 8) + i) & 0xffff
          addr = (__addr__ - (i & 0x100)) & 0xffff
          (__fetch__[addr][addr])
          @clk += 48
          ___pc__ += 2
          __data__ = ___x__ & ((__addr__ >> 8) + 1)
          __addr__ = (__data__ << 8) | (__addr__ & 0xff)
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0x9f # _sha
          addr = ___pc__ + 1
          i = ___y__ + (__fetch__[___pc__][___pc__])
          __addr__ = (((__fetch__[addr][addr]) << 8) + i) & 0xffff
          addr = (__addr__ - (i & 0x100)) & 0xffff
          (__fetch__[addr][addr])
          @clk += 48
          ___pc__ += 2
          __data__ = ___a__ & ___x__ & ((__addr__ >> 8) + 1)
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0xa0 # _ldy
          __data__ = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          @clk += 24
          @_p_nz = ___y__ = __data__
        when 0xa1 # _lda
          addr = (__fetch__[___pc__][___pc__]) + ___x__
          ___pc__ += 1
          @clk += 60
          __addr__ = __ram__[addr & 0xff] | __ram__[(addr + 1) & 0xff] << 8
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          @_p_nz = ___a__ = __data__
        when 0xa2 # _ldx
          __data__ = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          @clk += 24
          @_p_nz = ___x__ = __data__
        when 0xa3 # _lax
          addr = (__fetch__[___pc__][___pc__]) + ___x__
          ___pc__ += 1
          @clk += 60
          __addr__ = __ram__[addr & 0xff] | __ram__[(addr + 1) & 0xff] << 8
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          @_p_nz = ___a__ = ___x__ = __data__
        when 0xa4 # _ldy
          __addr__ = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          @clk += 36
          __data__ = __ram__[__addr__]
          @_p_nz = ___y__ = __data__
        when 0xa5 # _lda
          __addr__ = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          @clk += 36
          __data__ = __ram__[__addr__]
          @_p_nz = ___a__ = __data__
        when 0xa6 # _ldx
          __addr__ = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          @clk += 36
          __data__ = __ram__[__addr__]
          @_p_nz = ___x__ = __data__
        when 0xa7 # _lax
          __addr__ = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          @clk += 36
          __data__ = __ram__[__addr__]
          @_p_nz = ___a__ = ___x__ = __data__
        when 0xa8 # _tay
          @clk += 24
          @_p_nz = ___y__ = ___a__
        when 0xa9 # _lda
          __data__ = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          @clk += 24
          @_p_nz = ___a__ = __data__
        when 0xaa # _tax
          @clk += 24
          @_p_nz = ___x__ = ___a__
        when 0xab # _lxa
          __data__ = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          @clk += 24
          @_p_nz = ___a__ = ___x__ = __data__
        when 0xac # _ldy
          __addr__ = (__fetch__[___pc__][___pc__] + (__fetch__[___pc__ + 1][___pc__ + 1] << 8))
          ___pc__ += 2
          @clk += 36
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          @_p_nz = ___y__ = __data__
        when 0xad # _lda
          __addr__ = (__fetch__[___pc__][___pc__] + (__fetch__[___pc__ + 1][___pc__ + 1] << 8))
          ___pc__ += 2
          @clk += 36
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          @_p_nz = ___a__ = __data__
        when 0xae # _ldx
          __addr__ = (__fetch__[___pc__][___pc__] + (__fetch__[___pc__ + 1][___pc__ + 1] << 8))
          ___pc__ += 2
          @clk += 36
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          @_p_nz = ___x__ = __data__
        when 0xaf # _lax
          __addr__ = (__fetch__[___pc__][___pc__] + (__fetch__[___pc__ + 1][___pc__ + 1] << 8))
          ___pc__ += 2
          @clk += 36
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          @_p_nz = ___a__ = ___x__ = __data__
        when 0xb0 # _bcs
          if @_p_c != 0
            tmp = ___pc__ + 1
            rel = (__fetch__[___pc__][___pc__])
            ___pc__ = (tmp + (rel < 128 ? rel : rel | 0xff00)) & 0xffff
            @clk += tmp[8] == ___pc__[8] ? 36 : 48
          else
            ___pc__ += 1
            @clk += 24
          end
        when 0xb1 # _lda
          addr = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          indexed = __ram__[addr] + ___y__
          @clk += 48
          __addr__ = ((__ram__[(addr + 1) & 0xff] << 8) + indexed) & 0xffff
          if indexed & 0x100 != 0
            addr = (__addr__ - 0x100) & 0xffff # for inlining (__fetch__[addr][addr])
            (__fetch__[addr][addr])
            @clk += 12
          end
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          @_p_nz = ___a__ = __data__
        when 0xb2 # _jam
          ___pc__ = (___pc__ - 1) & 0xffff
          @clk += 24
          unless @jammed
            @jammed = true
            # interrupt reset
            @clk_nmi = 0xffffffff
            @clk_irq = 0xffffffff
            @irq_flags = 0
          end
        when 0xb3 # _lax
          addr = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          indexed = __ram__[addr] + ___y__
          @clk += 48
          __addr__ = ((__ram__[(addr + 1) & 0xff] << 8) + indexed) & 0xffff
          if indexed & 0x100 != 0
            addr = (__addr__ - 0x100) & 0xffff # for inlining (__fetch__[addr][addr])
            (__fetch__[addr][addr])
            @clk += 12
          end
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          @_p_nz = ___a__ = ___x__ = __data__
        when 0xb4 # _ldy
          __addr__ = (___x__ + (__fetch__[___pc__][___pc__])) & 0xff
          ___pc__ += 1
          @clk += 48
          __data__ = __ram__[__addr__]
          @_p_nz = ___y__ = __data__
        when 0xb5 # _lda
          __addr__ = (___x__ + (__fetch__[___pc__][___pc__])) & 0xff
          ___pc__ += 1
          @clk += 48
          __data__ = __ram__[__addr__]
          @_p_nz = ___a__ = __data__
        when 0xb6 # _ldx
          __addr__ = (___y__ + (__fetch__[___pc__][___pc__])) & 0xff
          ___pc__ += 1
          @clk += 48
          __data__ = __ram__[__addr__]
          @_p_nz = ___x__ = __data__
        when 0xb7 # _lax
          __addr__ = (___y__ + (__fetch__[___pc__][___pc__])) & 0xff
          ___pc__ += 1
          @clk += 48
          __data__ = __ram__[__addr__]
          @_p_nz = ___a__ = ___x__ = __data__
        when 0xb8 # _clv
          @clk += 24
          @_p_v = 0
        when 0xb9 # _lda
          addr = ___pc__ + 1
          i = ___y__ + (__fetch__[___pc__][___pc__])
          __addr__ = (((__fetch__[addr][addr]) << 8) + i) & 0xffff
          @clk += 36
          if i & 0x100 != 0
            addr = (__addr__ - 0x100) & 0xffff # for inlining (__fetch__[addr][addr])
            (__fetch__[addr][addr])
            @clk += 12
          end
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          ___pc__ += 2
          @_p_nz = ___a__ = __data__
        when 0xba # _tsx
          @clk += 24
          @_p_nz = ___x__ = ___sp__
        when 0xbb # _las
          addr = ___pc__ + 1
          i = ___y__ + (__fetch__[___pc__][___pc__])
          __addr__ = (((__fetch__[addr][addr]) << 8) + i) & 0xffff
          @clk += 36
          if i & 0x100 != 0
            addr = (__addr__ - 0x100) & 0xffff # for inlining (__fetch__[addr][addr])
            (__fetch__[addr][addr])
            @clk += 12
          end
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          ___pc__ += 2
          ___sp__ &= __data__
          @_p_nz = ___a__ = ___x__ = ___sp__
        when 0xbc # _ldy
          addr = ___pc__ + 1
          i = ___x__ + (__fetch__[___pc__][___pc__])
          __addr__ = (((__fetch__[addr][addr]) << 8) + i) & 0xffff
          @clk += 36
          if i & 0x100 != 0
            addr = (__addr__ - 0x100) & 0xffff # for inlining (__fetch__[addr][addr])
            (__fetch__[addr][addr])
            @clk += 12
          end
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          ___pc__ += 2
          @_p_nz = ___y__ = __data__
        when 0xbd # _lda
          addr = ___pc__ + 1
          i = ___x__ + (__fetch__[___pc__][___pc__])
          __addr__ = (((__fetch__[addr][addr]) << 8) + i) & 0xffff
          @clk += 36
          if i & 0x100 != 0
            addr = (__addr__ - 0x100) & 0xffff # for inlining (__fetch__[addr][addr])
            (__fetch__[addr][addr])
            @clk += 12
          end
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          ___pc__ += 2
          @_p_nz = ___a__ = __data__
        when 0xbe # _ldx
          addr = ___pc__ + 1
          i = ___y__ + (__fetch__[___pc__][___pc__])
          __addr__ = (((__fetch__[addr][addr]) << 8) + i) & 0xffff
          @clk += 36
          if i & 0x100 != 0
            addr = (__addr__ - 0x100) & 0xffff # for inlining (__fetch__[addr][addr])
            (__fetch__[addr][addr])
            @clk += 12
          end
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          ___pc__ += 2
          @_p_nz = ___x__ = __data__
        when 0xbf # _lax
          addr = ___pc__ + 1
          i = ___y__ + (__fetch__[___pc__][___pc__])
          __addr__ = (((__fetch__[addr][addr]) << 8) + i) & 0xffff
          @clk += 36
          if i & 0x100 != 0
            addr = (__addr__ - 0x100) & 0xffff # for inlining (__fetch__[addr][addr])
            (__fetch__[addr][addr])
            @clk += 12
          end
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          ___pc__ += 2
          @_p_nz = ___a__ = ___x__ = __data__
        when 0xc0 # _cpy
          __data__ = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          @clk += 24
          data = ___y__ - __data__
          @_p_nz = data & 0xff
          @_p_c = 1 - data[8]
        when 0xc1 # _cmp
          addr = (__fetch__[___pc__][___pc__]) + ___x__
          ___pc__ += 1
          @clk += 60
          __addr__ = __ram__[addr & 0xff] | __ram__[(addr + 1) & 0xff] << 8
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          data = ___a__ - __data__
          @_p_nz = data & 0xff
          @_p_c = 1 - data[8]
        when 0xc2 # _nop
          ___pc__ += 1
          @clk += 2 * RP2A03_CC
        when 0xc3 # _dcp
          addr = (__fetch__[___pc__][___pc__]) + ___x__
          ___pc__ += 1
          @clk += 60
          __addr__ = __ram__[addr & 0xff] | __ram__[(addr + 1) & 0xff] << 8
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
          __data__ = (__data__ - 1) & 0xff
          data = ___a__ - __data__
          @_p_nz = data & 0xff
          @_p_c = 1 - data[8]
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0xc4 # _cpy
          __addr__ = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          @clk += 36
          __data__ = __ram__[__addr__]
          data = ___y__ - __data__
          @_p_nz = data & 0xff
          @_p_c = 1 - data[8]
        when 0xc5 # _cmp
          __addr__ = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          @clk += 36
          __data__ = __ram__[__addr__]
          data = ___a__ - __data__
          @_p_nz = data & 0xff
          @_p_c = 1 - data[8]
        when 0xc6 # _dec
          __addr__ = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          @clk += 36
          __data__ = __ram__[__addr__]
          @clk += 24
          __data__ = @_p_nz = (__data__ - 1) & 0xff
          __ram__[__addr__] = __data__
        when 0xc7 # _dcp
          __addr__ = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          @clk += 36
          __data__ = __ram__[__addr__]
          @clk += 24
          __data__ = (__data__ - 1) & 0xff
          data = ___a__ - __data__
          @_p_nz = data & 0xff
          @_p_c = 1 - data[8]
          __ram__[__addr__] = __data__
        when 0xc8 # _iny
          @clk += 24
          __data__ = @_p_nz = ___y__ = (___y__ + 1) & 0xff
        when 0xc9 # _cmp
          __data__ = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          @clk += 24
          data = ___a__ - __data__
          @_p_nz = data & 0xff
          @_p_c = 1 - data[8]
        when 0xca # _dex
          @clk += 24
          __data__ = @_p_nz = ___x__ = (___x__ - 1) & 0xff
        when 0xcb # _sbx
          __data__ = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          @clk += 24
          __data__ = (___a__ & ___x__) - __data__
          @_p_c = (__data__ & 0xffff) <= 0xff ? 1 : 0
          @_p_nz = ___x__ = __data__ & 0xff
        when 0xcc # _cpy
          __addr__ = (__fetch__[___pc__][___pc__] + (__fetch__[___pc__ + 1][___pc__ + 1] << 8))
          ___pc__ += 2
          @clk += 36
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          data = ___y__ - __data__
          @_p_nz = data & 0xff
          @_p_c = 1 - data[8]
        when 0xcd # _cmp
          __addr__ = (__fetch__[___pc__][___pc__] + (__fetch__[___pc__ + 1][___pc__ + 1] << 8))
          ___pc__ += 2
          @clk += 36
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          data = ___a__ - __data__
          @_p_nz = data & 0xff
          @_p_c = 1 - data[8]
        when 0xce # _dec
          __addr__ = (__fetch__[___pc__][___pc__] + (__fetch__[___pc__ + 1][___pc__ + 1] << 8))
          ___pc__ += 2
          @clk += 36
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
          __data__ = @_p_nz = (__data__ - 1) & 0xff
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0xcf # _dcp
          __addr__ = (__fetch__[___pc__][___pc__] + (__fetch__[___pc__ + 1][___pc__ + 1] << 8))
          ___pc__ += 2
          @clk += 36
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
          __data__ = (__data__ - 1) & 0xff
          data = ___a__ - __data__
          @_p_nz = data & 0xff
          @_p_c = 1 - data[8]
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0xd0 # _bne
          if @_p_nz & 0xff != 0
            tmp = ___pc__ + 1
            rel = (__fetch__[___pc__][___pc__])
            ___pc__ = (tmp + (rel < 128 ? rel : rel | 0xff00)) & 0xffff
            @clk += tmp[8] == ___pc__[8] ? 36 : 48
          else
            ___pc__ += 1
            @clk += 24
          end
        when 0xd1 # _cmp
          addr = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          indexed = __ram__[addr] + ___y__
          @clk += 48
          __addr__ = ((__ram__[(addr + 1) & 0xff] << 8) + indexed) & 0xffff
          if indexed & 0x100 != 0
            addr = (__addr__ - 0x100) & 0xffff # for inlining (__fetch__[addr][addr])
            (__fetch__[addr][addr])
            @clk += 12
          end
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          data = ___a__ - __data__
          @_p_nz = data & 0xff
          @_p_c = 1 - data[8]
        when 0xd2 # _jam
          ___pc__ = (___pc__ - 1) & 0xffff
          @clk += 24
          unless @jammed
            @jammed = true
            # interrupt reset
            @clk_nmi = 0xffffffff
            @clk_irq = 0xffffffff
            @irq_flags = 0
          end
        when 0xd3 # _dcp
          addr = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          indexed = __ram__[addr] + ___y__
          @clk += 48
          @clk += 12
          __addr__ = (__ram__[(addr + 1) & 0xff] << 8) + indexed
          addr = __addr__ - (indexed & 0x100) # for inlining (__fetch__[addr][addr])
          (__fetch__[addr][addr])
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
          __data__ = (__data__ - 1) & 0xff
          data = ___a__ - __data__
          @_p_nz = data & 0xff
          @_p_c = 1 - data[8]
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0xd4 # _nop
          ___pc__ += 1
          @clk += 4 * RP2A03_CC
        when 0xd5 # _cmp
          __addr__ = (___x__ + (__fetch__[___pc__][___pc__])) & 0xff
          ___pc__ += 1
          @clk += 48
          __data__ = __ram__[__addr__]
          data = ___a__ - __data__
          @_p_nz = data & 0xff
          @_p_c = 1 - data[8]
        when 0xd6 # _dec
          __addr__ = (___x__ + (__fetch__[___pc__][___pc__])) & 0xff
          ___pc__ += 1
          @clk += 48
          __data__ = __ram__[__addr__]
          @clk += 24
          __data__ = @_p_nz = (__data__ - 1) & 0xff
          __ram__[__addr__] = __data__
        when 0xd7 # _dcp
          __addr__ = (___x__ + (__fetch__[___pc__][___pc__])) & 0xff
          ___pc__ += 1
          @clk += 48
          __data__ = __ram__[__addr__]
          @clk += 24
          __data__ = (__data__ - 1) & 0xff
          data = ___a__ - __data__
          @_p_nz = data & 0xff
          @_p_c = 1 - data[8]
          __ram__[__addr__] = __data__
        when 0xd8 # _cld
          @clk += 24
          @_p_d = 0
        when 0xd9 # _cmp
          addr = ___pc__ + 1
          i = ___y__ + (__fetch__[___pc__][___pc__])
          __addr__ = (((__fetch__[addr][addr]) << 8) + i) & 0xffff
          @clk += 36
          if i & 0x100 != 0
            addr = (__addr__ - 0x100) & 0xffff # for inlining (__fetch__[addr][addr])
            (__fetch__[addr][addr])
            @clk += 12
          end
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          ___pc__ += 2
          data = ___a__ - __data__
          @_p_nz = data & 0xff
          @_p_c = 1 - data[8]
        when 0xda # _nop
          ___pc__ += 0
          @clk += 2 * RP2A03_CC
        when 0xdb # _dcp
          addr = ___pc__ + 1
          i = ___y__ + (__fetch__[___pc__][___pc__])
          __addr__ = (((__fetch__[addr][addr]) << 8) + i) & 0xffff
          addr = (__addr__ - (i & 0x100)) & 0xffff
          (__fetch__[addr][addr])
          @clk += 48
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
          ___pc__ += 2
          __data__ = (__data__ - 1) & 0xff
          data = ___a__ - __data__
          @_p_nz = data & 0xff
          @_p_c = 1 - data[8]
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0xdc # _nop
          addr = ___pc__ + 1
          i = ___x__ + (__fetch__[___pc__][___pc__])
          __addr__ = (((__fetch__[addr][addr]) << 8) + i) & 0xffff
          @clk += 36
          if i & 0x100 != 0
            addr = (__addr__ - 0x100) & 0xffff # for inlining (__fetch__[addr][addr])
            (__fetch__[addr][addr])
            @clk += 12
          end
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          ___pc__ += 2
        when 0xdd # _cmp
          addr = ___pc__ + 1
          i = ___x__ + (__fetch__[___pc__][___pc__])
          __addr__ = (((__fetch__[addr][addr]) << 8) + i) & 0xffff
          @clk += 36
          if i & 0x100 != 0
            addr = (__addr__ - 0x100) & 0xffff # for inlining (__fetch__[addr][addr])
            (__fetch__[addr][addr])
            @clk += 12
          end
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          ___pc__ += 2
          data = ___a__ - __data__
          @_p_nz = data & 0xff
          @_p_c = 1 - data[8]
        when 0xde # _dec
          addr = ___pc__ + 1
          i = ___x__ + (__fetch__[___pc__][___pc__])
          __addr__ = (((__fetch__[addr][addr]) << 8) + i) & 0xffff
          addr = (__addr__ - (i & 0x100)) & 0xffff
          (__fetch__[addr][addr])
          @clk += 48
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
          ___pc__ += 2
          __data__ = @_p_nz = (__data__ - 1) & 0xff
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0xdf # _dcp
          addr = ___pc__ + 1
          i = ___x__ + (__fetch__[___pc__][___pc__])
          __addr__ = (((__fetch__[addr][addr]) << 8) + i) & 0xffff
          addr = (__addr__ - (i & 0x100)) & 0xffff
          (__fetch__[addr][addr])
          @clk += 48
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
          ___pc__ += 2
          __data__ = (__data__ - 1) & 0xff
          data = ___a__ - __data__
          @_p_nz = data & 0xff
          @_p_c = 1 - data[8]
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0xe0 # _cpx
          __data__ = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          @clk += 24
          data = ___x__ - __data__
          @_p_nz = data & 0xff
          @_p_c = 1 - data[8]
        when 0xe1 # _sbc
          addr = (__fetch__[___pc__][___pc__]) + ___x__
          ___pc__ += 1
          @clk += 60
          __addr__ = __ram__[addr & 0xff] | __ram__[(addr + 1) & 0xff] << 8
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          data = __data__ ^ 0xff
          tmp = ___a__ + data + @_p_c
          @_p_v = ~(___a__ ^ data) & (___a__ ^ tmp) & 0x80
          @_p_nz = ___a__ = tmp & 0xff
          @_p_c = tmp[8]
        when 0xe2 # _nop
          ___pc__ += 1
          @clk += 2 * RP2A03_CC
        when 0xe3 # _isb
          addr = (__fetch__[___pc__][___pc__]) + ___x__
          ___pc__ += 1
          @clk += 60
          __addr__ = __ram__[addr & 0xff] | __ram__[(addr + 1) & 0xff] << 8
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
          __data__ = (__data__ + 1) & 0xff
          data = __data__ ^ 0xff
          tmp = ___a__ + data + @_p_c
          @_p_v = ~(___a__ ^ data) & (___a__ ^ tmp) & 0x80
          @_p_nz = ___a__ = tmp & 0xff
          @_p_c = tmp[8]
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0xe4 # _cpx
          __addr__ = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          @clk += 36
          __data__ = __ram__[__addr__]
          data = ___x__ - __data__
          @_p_nz = data & 0xff
          @_p_c = 1 - data[8]
        when 0xe5 # _sbc
          __addr__ = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          @clk += 36
          __data__ = __ram__[__addr__]
          data = __data__ ^ 0xff
          tmp = ___a__ + data + @_p_c
          @_p_v = ~(___a__ ^ data) & (___a__ ^ tmp) & 0x80
          @_p_nz = ___a__ = tmp & 0xff
          @_p_c = tmp[8]
        when 0xe6 # _inc
          __addr__ = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          @clk += 36
          __data__ = __ram__[__addr__]
          @clk += 24
          __data__ = @_p_nz = (__data__ + 1) & 0xff
          __ram__[__addr__] = __data__
        when 0xe7 # _isb
          __addr__ = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          @clk += 36
          __data__ = __ram__[__addr__]
          @clk += 24
          __data__ = (__data__ + 1) & 0xff
          data = __data__ ^ 0xff
          tmp = ___a__ + data + @_p_c
          @_p_v = ~(___a__ ^ data) & (___a__ ^ tmp) & 0x80
          @_p_nz = ___a__ = tmp & 0xff
          @_p_c = tmp[8]
          __ram__[__addr__] = __data__
        when 0xe8 # _inx
          @clk += 24
          __data__ = @_p_nz = ___x__ = (___x__ + 1) & 0xff
        when 0xe9 # _sbc
          __data__ = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          @clk += 24
          data = __data__ ^ 0xff
          tmp = ___a__ + data + @_p_c
          @_p_v = ~(___a__ ^ data) & (___a__ ^ tmp) & 0x80
          @_p_nz = ___a__ = tmp & 0xff
          @_p_c = tmp[8]
        when 0xea # _nop
          ___pc__ += 0
          @clk += 2 * RP2A03_CC
        when 0xeb # _sbc
          __data__ = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          @clk += 24
          data = __data__ ^ 0xff
          tmp = ___a__ + data + @_p_c
          @_p_v = ~(___a__ ^ data) & (___a__ ^ tmp) & 0x80
          @_p_nz = ___a__ = tmp & 0xff
          @_p_c = tmp[8]
        when 0xec # _cpx
          __addr__ = (__fetch__[___pc__][___pc__] + (__fetch__[___pc__ + 1][___pc__ + 1] << 8))
          ___pc__ += 2
          @clk += 36
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          data = ___x__ - __data__
          @_p_nz = data & 0xff
          @_p_c = 1 - data[8]
        when 0xed # _sbc
          __addr__ = (__fetch__[___pc__][___pc__] + (__fetch__[___pc__ + 1][___pc__ + 1] << 8))
          ___pc__ += 2
          @clk += 36
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          data = __data__ ^ 0xff
          tmp = ___a__ + data + @_p_c
          @_p_v = ~(___a__ ^ data) & (___a__ ^ tmp) & 0x80
          @_p_nz = ___a__ = tmp & 0xff
          @_p_c = tmp[8]
        when 0xee # _inc
          __addr__ = (__fetch__[___pc__][___pc__] + (__fetch__[___pc__ + 1][___pc__ + 1] << 8))
          ___pc__ += 2
          @clk += 36
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
          __data__ = @_p_nz = (__data__ + 1) & 0xff
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0xef # _isb
          __addr__ = (__fetch__[___pc__][___pc__] + (__fetch__[___pc__ + 1][___pc__ + 1] << 8))
          ___pc__ += 2
          @clk += 36
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
          __data__ = (__data__ + 1) & 0xff
          data = __data__ ^ 0xff
          tmp = ___a__ + data + @_p_c
          @_p_v = ~(___a__ ^ data) & (___a__ ^ tmp) & 0x80
          @_p_nz = ___a__ = tmp & 0xff
          @_p_c = tmp[8]
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0xf0 # _beq
          if @_p_nz & 0xff == 0
            tmp = ___pc__ + 1
            rel = (__fetch__[___pc__][___pc__])
            ___pc__ = (tmp + (rel < 128 ? rel : rel | 0xff00)) & 0xffff
            @clk += tmp[8] == ___pc__[8] ? 36 : 48
          else
            ___pc__ += 1
            @clk += 24
          end
        when 0xf1 # _sbc
          addr = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          indexed = __ram__[addr] + ___y__
          @clk += 48
          __addr__ = ((__ram__[(addr + 1) & 0xff] << 8) + indexed) & 0xffff
          if indexed & 0x100 != 0
            addr = (__addr__ - 0x100) & 0xffff # for inlining (__fetch__[addr][addr])
            (__fetch__[addr][addr])
            @clk += 12
          end
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          data = __data__ ^ 0xff
          tmp = ___a__ + data + @_p_c
          @_p_v = ~(___a__ ^ data) & (___a__ ^ tmp) & 0x80
          @_p_nz = ___a__ = tmp & 0xff
          @_p_c = tmp[8]
        when 0xf2 # _jam
          ___pc__ = (___pc__ - 1) & 0xffff
          @clk += 24
          unless @jammed
            @jammed = true
            # interrupt reset
            @clk_nmi = 0xffffffff
            @clk_irq = 0xffffffff
            @irq_flags = 0
          end
        when 0xf3 # _isb
          addr = (__fetch__[___pc__][___pc__])
          ___pc__ += 1
          indexed = __ram__[addr] + ___y__
          @clk += 48
          @clk += 12
          __addr__ = (__ram__[(addr + 1) & 0xff] << 8) + indexed
          addr = __addr__ - (indexed & 0x100) # for inlining (__fetch__[addr][addr])
          (__fetch__[addr][addr])
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
          __data__ = (__data__ + 1) & 0xff
          data = __data__ ^ 0xff
          tmp = ___a__ + data + @_p_c
          @_p_v = ~(___a__ ^ data) & (___a__ ^ tmp) & 0x80
          @_p_nz = ___a__ = tmp & 0xff
          @_p_c = tmp[8]
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0xf4 # _nop
          ___pc__ += 1
          @clk += 4 * RP2A03_CC
        when 0xf5 # _sbc
          __addr__ = (___x__ + (__fetch__[___pc__][___pc__])) & 0xff
          ___pc__ += 1
          @clk += 48
          __data__ = __ram__[__addr__]
          data = __data__ ^ 0xff
          tmp = ___a__ + data + @_p_c
          @_p_v = ~(___a__ ^ data) & (___a__ ^ tmp) & 0x80
          @_p_nz = ___a__ = tmp & 0xff
          @_p_c = tmp[8]
        when 0xf6 # _inc
          __addr__ = (___x__ + (__fetch__[___pc__][___pc__])) & 0xff
          ___pc__ += 1
          @clk += 48
          __data__ = __ram__[__addr__]
          @clk += 24
          __data__ = @_p_nz = (__data__ + 1) & 0xff
          __ram__[__addr__] = __data__
        when 0xf7 # _isb
          __addr__ = (___x__ + (__fetch__[___pc__][___pc__])) & 0xff
          ___pc__ += 1
          @clk += 48
          __data__ = __ram__[__addr__]
          @clk += 24
          __data__ = (__data__ + 1) & 0xff
          data = __data__ ^ 0xff
          tmp = ___a__ + data + @_p_c
          @_p_v = ~(___a__ ^ data) & (___a__ ^ tmp) & 0x80
          @_p_nz = ___a__ = tmp & 0xff
          @_p_c = tmp[8]
          __ram__[__addr__] = __data__
        when 0xf8 # _sed
          @clk += 24
          @_p_d = 8
        when 0xf9 # _sbc
          addr = ___pc__ + 1
          i = ___y__ + (__fetch__[___pc__][___pc__])
          __addr__ = (((__fetch__[addr][addr]) << 8) + i) & 0xffff
          @clk += 36
          if i & 0x100 != 0
            addr = (__addr__ - 0x100) & 0xffff # for inlining (__fetch__[addr][addr])
            (__fetch__[addr][addr])
            @clk += 12
          end
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          ___pc__ += 2
          data = __data__ ^ 0xff
          tmp = ___a__ + data + @_p_c
          @_p_v = ~(___a__ ^ data) & (___a__ ^ tmp) & 0x80
          @_p_nz = ___a__ = tmp & 0xff
          @_p_c = tmp[8]
        when 0xfa # _nop
          ___pc__ += 0
          @clk += 2 * RP2A03_CC
        when 0xfb # _isb
          addr = ___pc__ + 1
          i = ___y__ + (__fetch__[___pc__][___pc__])
          __addr__ = (((__fetch__[addr][addr]) << 8) + i) & 0xffff
          addr = (__addr__ - (i & 0x100)) & 0xffff
          (__fetch__[addr][addr])
          @clk += 48
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
          ___pc__ += 2
          __data__ = (__data__ + 1) & 0xff
          data = __data__ ^ 0xff
          tmp = ___a__ + data + @_p_c
          @_p_v = ~(___a__ ^ data) & (___a__ ^ tmp) & 0x80
          @_p_nz = ___a__ = tmp & 0xff
          @_p_c = tmp[8]
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0xfc # _nop
          addr = ___pc__ + 1
          i = ___x__ + (__fetch__[___pc__][___pc__])
          __addr__ = (((__fetch__[addr][addr]) << 8) + i) & 0xffff
          @clk += 36
          if i & 0x100 != 0
            addr = (__addr__ - 0x100) & 0xffff # for inlining (__fetch__[addr][addr])
            (__fetch__[addr][addr])
            @clk += 12
          end
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          ___pc__ += 2
        when 0xfd # _sbc
          addr = ___pc__ + 1
          i = ___x__ + (__fetch__[___pc__][___pc__])
          __addr__ = (((__fetch__[addr][addr]) << 8) + i) & 0xffff
          @clk += 36
          if i & 0x100 != 0
            addr = (__addr__ - 0x100) & 0xffff # for inlining (__fetch__[addr][addr])
            (__fetch__[addr][addr])
            @clk += 12
          end
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          ___pc__ += 2
          data = __data__ ^ 0xff
          tmp = ___a__ + data + @_p_c
          @_p_v = ~(___a__ ^ data) & (___a__ ^ tmp) & 0x80
          @_p_nz = ___a__ = tmp & 0xff
          @_p_c = tmp[8]
        when 0xfe # _inc
          addr = ___pc__ + 1
          i = ___x__ + (__fetch__[___pc__][___pc__])
          __addr__ = (((__fetch__[addr][addr]) << 8) + i) & 0xffff
          addr = (__addr__ - (i & 0x100)) & 0xffff
          (__fetch__[addr][addr])
          @clk += 48
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
          ___pc__ += 2
          __data__ = @_p_nz = (__data__ + 1) & 0xff
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        when 0xff # _isb
          addr = ___pc__ + 1
          i = ___x__ + (__fetch__[___pc__][___pc__])
          __addr__ = (((__fetch__[addr][addr]) << 8) + i) & 0xffff
          addr = (__addr__ - (i & 0x100)) & 0xffff
          (__fetch__[addr][addr])
          @clk += 48
          __data__ = (__fetch__[__addr__][__addr__])
          @clk += 12
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
          ___pc__ += 2
          __data__ = (__data__ + 1) & 0xff
          data = __data__ ^ 0xff
          tmp = ___a__ + data + @_p_c
          @_p_v = ~(___a__ ^ data) & (___a__ ^ tmp) & 0x80
          @_p_nz = ___a__ = tmp & 0xff
          @_p_c = tmp[8]
          (__store__[__addr__][__addr__, __data__])
          @clk += 12
        end

        if @ppu_sync
          @ppu.sync(@clk)
        end
      end while @clk < @clk_target
      clock = @apu.do_clock

      if clock > @clk_frame
        clock = @clk_frame
      end

      if @clk < @clk_nmi
        if clock > @clk_nmi
          clock = @clk_nmi
        end
        if @clk < @clk_irq
          if clock > @clk_irq
            clock = @clk_irq
          end
        else
          @clk_irq = 0xffffffff
          if @jammed
            return
          end
          __ram__[0x0100 + ___sp__] = ___pc__ >> 8
          ___sp__ = (___sp__ - 1) & 0xff
          __ram__[0x0100 + ___sp__] = ___pc__ & 0xff
          ___sp__ = (___sp__ - 1) & 0xff
          __ram__[0x0100 + ___sp__] = flags_pack
          ___sp__ = (___sp__ - 1) & 0xff
          @_p_i = 0x04
          @clk += 84
          addr = IRQ_VECTOR == NMI_VECTOR ? NMI_VECTOR : fetch_irq_isr_vector
          ___pc__ = (__fetch__[addr][addr] + (__fetch__[addr + 1][addr + 1] << 8))
        end
      else
        @clk_nmi = @clk_irq = 0xffffffff
        if @jammed
          return
        end
        __ram__[0x0100 + ___sp__] = ___pc__ >> 8
        ___sp__ = (___sp__ - 1) & 0xff
        __ram__[0x0100 + ___sp__] = ___pc__ & 0xff
        ___sp__ = (___sp__ - 1) & 0xff
        __ram__[0x0100 + ___sp__] = flags_pack
        ___sp__ = (___sp__ - 1) & 0xff
        @_p_i = 0x04
        @clk += 84
        addr = NMI_VECTOR == NMI_VECTOR ? NMI_VECTOR : fetch_irq_isr_vector
        ___pc__ = (__fetch__[addr][addr] + (__fetch__[addr + 1][addr + 1] << 8))
      end
      @clk_target = clock
    end while @clk < @clk_frame
  ensure
    @addr = __addr__
    @data = __data__
    @_a = ___a__
    @_x = ___x__
    @_y = ___y__
    @_pc = ___pc__
    @_sp = ___sp__
    @fetch = __fetch__
    @store = __store__
    @ram = __ram__
    @opcode = __opcode__
  end
end
