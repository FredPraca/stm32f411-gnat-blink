------------------------------------------------------------------------------
--                                                                          --
--                             GNAT EXAMPLE                                 --
--                                                                          --
--             Copyright (C) 2014, Free Software Foundation, Inc.           --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;

with System.STM32F4; use System.STM32F4;

with System.STM32F4.GPIO; use System.STM32F4.GPIO;

with System.STM32F4.Reset_Clock_Control;
use System.STM32F4.Reset_Clock_Control;

package body LEDs is

   All_LEDs_On  : constant Word := 2**5;

   All_LEDs_Off : constant Word := Shift_Left (All_LEDs_On, 16);

   procedure All_Off is
   begin
      GPIOA.BSRR := All_LEDs_Off;
   end All_Off;


   procedure All_On is
   begin
      GPIOA.BSRR := GPIOA.BSRR or All_LEDs_On;
   end All_On;

   procedure Initialize is
   begin
      --  Enable clock for GPIO-A
      RCC.RCC_AHB1ENR.GPIOA_Clock_Enable := True;

      --  Configure PA5
      GPIOA.MODER   (5) := Mode_OUT;
      GPIOA.OTYPER  (5) := Type_PP;
      GPIOA.OSPEEDR (5) := Speed_100MHz;
      GPIOA.PUPDR   (5) := No_Pull;
   end Initialize;

begin
   Initialize;
end LEDs;
