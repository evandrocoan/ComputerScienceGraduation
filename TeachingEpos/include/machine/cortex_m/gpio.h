// EPOS Cortex-M GPIO Mediator Declarations

#ifndef __cortex_m_gpio_h_
#define __cortex_m_gpio_h_

#include <machine.h>
#include <ic.h>
#include <gpio.h>

__BEGIN_SYS

class Cortex_M_GPIO: private GPIO_Common, private Cortex_M_Model
{
private:
    static const bool supports_power_up = Cortex_M_Model::supports_gpio_power_up;

public:
    enum Level {
        HIGH,
        LOW,
    };
    enum Edge {
        RISING,
        FALLING,
        BOTH,
    };
    enum Direction {
        INPUT,
        OUTPUT,
    };

    Cortex_M_GPIO(char port, unsigned int pin, const Direction & dir, const IC::Interrupt_Handler & handler = 0)
    : _port(port - 'A'), _pin(pin), _pin_bit(1 << pin), _data(&gpio(_port, _pin_bit << 2)), _handler(handler) {
        assert((port >= 'A') && (port <= 'A' + GPIO_PORTS));
        gpio(_port, AFSEL) &= ~_pin_bit; // Set pin as software controlled
        if(dir == OUTPUT)
            output();
        else
            input();
        clear_interrupt();
        if(handler)
            int_enable();
    }

    bool get() const { return *_data; }
    void set(bool value) { *_data = 0xff * value; }

    void output() { gpio(_port, DIR) |= _pin_bit; }
    void input() { gpio(_port, DIR) &= ~_pin_bit; }

    void pull_up() { gpio_pull_up(_port, _pin); }
    void pull_down() { gpio_pull_down(_port, _pin); }

    void int_enable() { gpio(_port, IM) |= _pin_bit; }
    //void int_enable(const Level & level, bool power_up = false, const Level & power_up_level = HIGH); // TODO
    void int_enable(const Edge & edge, bool power_up = false, const Edge & power_up_edge = RISING);
    void int_disable() { gpio(_port, IM) &= ~_pin_bit; }
    void int_handler(const IC::Interrupt_Handler & h) {
        _handler = h;
        _devices[_port][_pin] = this;
    }

private:
    void clear_interrupt() {
        gpio(_port, ICR) = _pin_bit; 
        gpio(_port, IRQ_DETECT_ACK) &= ~(_pin_bit << (8 * _port));
    }

    static void handle_int(const IC::Interrupt_Id & i);

private:
    unsigned char _port;
    unsigned char _pin;
    unsigned int _pin_bit;
    volatile Reg32 * _data;
    IC::Interrupt_Handler _handler;

    static Cortex_M_GPIO * _devices[GPIO_PORTS][8];
};

__END_SYS

#endif
