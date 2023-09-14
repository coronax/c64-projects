// Written by Nick Gammon
// February 2011


#include <SPI.h>

char buf [100];
char newbuf[100];
volatile byte pos;
volatile boolean process_it;

char input_queue = 0; // a 1-byte queue? slacker!

void setup (void)
{

  Serial.begin (115200);   // debugging
  Serial.println ("arduino spi comm tester");

  // have to send on master in, *slave out*
  pinMode(MISO, OUTPUT);
  pinMode(MOSI, INPUT);
  pinMode(10,INPUT);
  pinMode(13,INPUT);
  
  // turn on SPI in slave mode
  SPCR |= _BV(SPE);
  
  // get ready for an interrupt 
  pos = 0;   // buffer empty
  process_it = false;

  // now turn on interrupts
  SPI.attachInterrupt();

}  // end of setup

bool interrupted = false;
// SPI interrupt routine
ISR (SPI_STC_vect)
{
  byte c = SPDR;  // grab byte from SPI Data Register
  interrupted = true;
  // add to buffer if room
  if (pos < sizeof buf)
  {
    if (c == '\r')
      c = '\n';
    buf [pos++] = c;
    
    if (pos > 0)
      process_it = true;
      
    SPDR = input_queue;
    input_queue = 0;
      
  }
}

// main loop - wait for flag set in interrupt routine
void loop (void)
{
  if (interrupted)
  {
    //Serial.println ("interrupted!");
    interrupted = false;
  }
  
  
  if (Serial.available() && input_queue == 0)
    input_queue = Serial.read();
  
  noInterrupts();
  if (process_it)
  {
    buf [pos] = 0;  
    strcpy (newbuf, buf);
    pos = 0;
    process_it = false;
  }
  interrupts();
      
  //Serial.print (buf);
  for (int i = 0; newbuf[i] != 0; ++i)
  {
    switch (newbuf[i])
    {
    case '\n':
      Serial.println ("");
      break;
    case 20:  // backspace
      Serial.print ("\x7f");//"\x08\x7f");
      break;
    case 28:  // red
      Serial.print ("\x1b\x5b""31m");
      break;
    case 5:  // white
      Serial.print ("\x1b\x5b""37m");
      break;
    case 145: // cursor up
      Serial.print ("\x1b\x5b""1A");
      break;
    case 17: // cursor down
      Serial.print ("\x1b\x5b""1B");
      break;
    case 157: // cursor left
      Serial.print ("\x1b\x5b""1D");
      break;
    case 29: // cursor right
      Serial.print ("\x1b\x5b""1C");
      break;
    default:
      Serial.print(buf[i]);
    } 
  }
  newbuf[0] = 0;
    
}  // end of loop
