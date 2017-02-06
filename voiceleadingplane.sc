(
var numrowscols=25;
var radius=13;
var hor_instrument = nil;
var ver_instrument = nil;
var nametosymbol = Dictionary.newFrom(["FMTrumpet", \fmtrumpet, "Pad", \pad]);
var nametoamplitude = Dictionary.newFrom(["FMTrumpet", -20, "Pad", 0.2]);

// define a function to convert a midi note number to a midi note name
var lastclickedrow = nil;
var lastclickedcol = nil;
var topleftnote, toprightnote, bottomleftnote, bottomrightnote;
var miditoname = ({ arg note = 60, style = \American ;
	var offset = 0 ;
	var midi, notes;
	case { style == \French } { offset = -1}
	{ style == \German } { offset = -3} ;
	midi = (note + 0.5).asInteger;
	notes = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"];

	(notes[midi%12] ++ (midi.div(12)-1+offset))
});

// define a function to convert a midi note name to a midi note number
var nametomidi = ({ arg name = "C4", style = \American ;
	var offset = 0 ; // French usage: +1 ; German usage: +3
	var twelves, ones, octaveIndex, midis;

	case { style == \French } { offset = 1}
	{ style == \German } { offset = 3} ;

	midis = Dictionary[($c->0),($d->2),($e->4),($f->5),($g->7),($a->9),($b->11)];
	ones = midis.at(name[0].toLower);

	if( (name[1].isDecDigit), {
		octaveIndex = 1;
	},{
		octaveIndex = 2;
		if( (name[1] == $#) || (name[1].toLower == $s) || (name[1] == $+), {
			ones = ones + 1;
		},{
			if( (name[1] == $b) || (name[1].toLower == $f) || (name[1] == $-), {
				ones = ones - 1;
			});
		});
	});
	twelves = (name.copyRange(octaveIndex, name.size).asInteger) * 12;

	(twelves + 12 + ones + (offset*12))
});



s.waitForBoot({
	var colorlut = [
		Color.new255(255,0,0),
		Color.new255(255,127,0),
		Color.new255(255,255,0),
		Color.new255(0,255,0),
		Color.new255(0,0,255),
		Color.new255(75,0,130),
		Color.new255(143,0,255),
		Color.new255(75,0,130),
		Color.new255(0,0,255),
		Color.new255(0,255,0),
		Color.new255(255,255,0),
		Color.new255(255,127,0),
		Color.new255(255,0,0)
	];
	var center_to_row_col = ();

	// FM Trumpet

	// Realization a Frequency Modulation Trumpet
	// Based on Dodge p.129, an adaption of Dexter Morrill's design

	(
		SynthDef(\fmtrumpet, {arg dur = 1, amp = -3, freq = 440, index = 1, halfsine = 1;
			var env, amp1, amp2, formant, dev1, dev2, rand, vibamp, vibosc, porta, vib, ac1env,
			ac2env, modenv, car1 ,car2, mod, mod2;
			env = EnvGen.kr(Env([0.01, 1, 1, 0.01], [0.01, dur - 0.02, 0.01], \exp), doneAction: 2);
			amp1 = amp.dbamp * 0.7; // amp of the first carrier
			amp2 = amp1 * 0.2; // amp of the second carrier. It is the second carrier
			// that will simulate the formant

			// The equation below is a standard way of calculating a value for frequency
			// which will be as near to a formant frequency as possible while also
			// being a harmonic partial of the fundamental. The important formant
			// for the trumpet is approximately 1500 Hz. Therefore we want to have an
			// increase in energy near that frequency to simulate the formant in order
			// to synthesize a trumpet-like sound. To do that we will use a second carrier
			// oscillator with a frequency corresponding to the formant. The trumpet is an
			// instrument with harmomic partials so the formant must also be a harmonic.
			// For example, suppose the fundamental frequency were 440Hz. It is easy to see
			// that 1500Hz is not a harmonic partial of 440Hz (1500 is not an integer
			// multiple of 440). If we put 440 into the equation:

			//	 formant =  (1500/440).round(1) * 440

			// we get a result of 1320 for formant. The "round" function:

			//	 a.round(b)

			// returns a value for a rounded to the nearest b. So (1500/440).round(1) would
			// return 3 instead of 3.409.
			// We do this because we want to multiply the fundamental by an integer to
			// get a frequency that will be a harmonic partial.
			// We then multiply the fundemental by that integer to get the
			// value of that harmonic in Hz as was done above to get 1320Hz. It might
			// help you understand this better if you insert your own values for
			// fundamentals and formants and do the calculations to see the results!

			formant = (1500 / freq).round(1) * freq;

			// The variable "dev1" is the deviation, or amplitude of the the modulating
			// oscillator. index is an index-like scalar set in the score to make the sound
			// more or less bright.

			dev1 = (freq * 3) * index;

			// The variable dev2 will be used to scale the output of the modulator
			// before being used by the second carrier. Because we don't want to create
			// as many side bands around the second carrier (the formant), the deviation
			// should be smaller.

			dev2 = 0.666 * index;

			// VIBRATO

			// a random component with interpolating LFNoise1

			rand = LFNoise1.ar(15, 0.007);

			// vibrato component amplitude.  This will read from a half sine shape from
			// a buffer stored in memory.  To make sure it only reads once, its frequency
			// will be the reciprocal of the duration.

			vibamp = Osc.kr(halfsine, dur.reciprocal, 0, 0.007);

			// vibrato component main oscillator

			vibosc = SinOsc.ar(5, 0, vibamp);

			// pitch slew function

			porta = EnvGen.kr(Env([0, 0.03, 0.02], [0.06, dur - 0.06], \lin));

			// multiply the three components together with 1 added to
			// create a frequency multiplier centered around 1

			vib	= (1 + rand) * (1 + vibosc) * (1 + porta);

			// ENVELOPES

			ac1env = EnvGen.kr(Env([0.001, 1, 0.8, 0.001], [0.1, dur - 0.25, 0.15], \exp));
			ac2env = EnvGen.kr(Env([0.001, 1, 0.8, 0.001], [0.1, dur - 0.45, 0.3], \exp));
			modenv = EnvGen.kr(Env([0.001, 1, 0.8, 0.001], [0.1, dur - 0.11, 0.01], \exp));

			// MODULATOR

			// The C:M ratio is 1:1, so fm = fc * vibrato

			mod = SinOsc.ar(freq * vib, 0, dev1 * modenv);

			// CARRIER1

			// fc*vib gives the fundamental frequency and then
			// we add the modulator signal

			car1 = SinOsc.ar((freq * vib) + mod, 0, ac1env * amp1);

			// The modulation deviation is scaled back before
			// applying it to the formant carrier, giving
			// fewer sidebands

			mod2 = mod * dev2;

			// The C:M ratio for the formant carrier is different
			// from the fundamental modulator, but since the
			// formant is harmonic to fc, the resulting formant:M
			// ratio is also harmonic, and in the reduced ratio
			// M will still be 1 -- so the same spectral components
			// will be present.

			// CARRIER2

			car2 = SinOsc.ar((formant * vib) + mod2, 0, amp2 * ac2env);

			Out.ar(0, (car1 + car2) * env!2);
		}).add;

SynthDef(\pad, { | freq, amp, dur, detune=#[1, 0.998, 0.999, 1.001, 1.002], cutofffreq=0.1, cutofflow=400, cutoffhigh=450|
		var sig, env, totalsig;
		var cutoffavg= (cutofflow+cutoffhigh)/2;
		var cutoffdiff= cutoffhigh-cutofflow;
		var attack = min(1,dur/2);
		var release= min(1,dur/2)*2;
		sig = LPF.ar(Saw.ar(freq*detune), ((SinOsc.kr(cutofffreq)*cutoffavg)+(cutoffavg+cutofflow)));
		env = Env.linen(attack, dur-(attack+release), release);
		totalsig = amp*EnvGen.kr(env, doneAction:2)*sig;

		Out.ar(0, Splay.ar(totalsig));
}).add;
	);

s.sync;

~i1 = Synth(\fmtrumpet, [\amp, nametoamplitude["FMTrumpet"], \dur, 1000, \freq, 0]);
~i2 = Synth(\fmtrumpet, [\amp, nametoamplitude["FMTrumpet"], \dur, 1000, \freq, 0]);

(
	w = Window.new(bounds:Rect(200,200,1000,1000));
	w.view.background_(Color.white);
	v = UserView(w, w.view.bounds.insetBy(5,5));
	v.animate = false;
	w.acceptsMouseOver_(true);
	topleftnote = TextField();
	bottomleftnote = TextField();
    hor_instrument = PopUpMenu(w, Rect(0,0,0,0));
    hor_instrument.items = ["FMTrumpet", "Pad"];
    hor_instrument.action = { | menu |
	   ~i1.free;
	   ~i1 = Synth(nametosymbol[menu.item], [\amp, nametoamplitude[menu.item], \dur, 1000, \freq, 0]);
	   if (lastclickedrow != nil)
	   {
		    var rownote = lastclickedrow.linlin(0,
               numrowscols-1,
			   nametomidi.value(bottomleftnote.string),
			   nametomidi.value(toprightnote.string));
		    ~i1.set(\freq, rownote.midicps);
			~i1.set(\amp, nametoamplitude[menu.item]);
	   };
    };
	toprightnote= TextField();
	bottomrightnote = TextField();
    ver_instrument = PopUpMenu(w, Rect(0,0,0,0));
    ver_instrument.items = ["FMTrumpet", "Pad"];
    ver_instrument.action = { | menu |
	    ~i2.free;
		~i2 = Synth(nametosymbol[menu.item], [\amp, nametoamplitude[menu.item], \dur, 1000, \freq, 0]);
	    if (lastclickedcol != nil)
	    {
		   var colnote = lastclickedcol.linlin(0,
		       numrowscols-1,
			   nametomidi.value(bottomrightnote.string),
			   nametomidi.value(topleftnote.string));
		   ~i2.set(\freq, colnote.midicps);
		   ~i2.set(\amp, nametoamplitude[menu.item]);
   	    };
    };

	v.layout = VLayout(
		HLayout(
			GridLayout.rows(
				[[topleftnote.string_("C5").align_(\center), columns:2],
					[toprightnote.string_("C5").align_(\center), columns:2],
				    [hor_instrument], nil],
				[[bottomleftnote.string_("C3").align_(\center), columns:2],
					[bottomrightnote.string_("C3").align_(\center), columns:2],
					[ver_instrument], nil],
		), nil),
		nil
	);

	v.drawFunc = { |v|
		Pen.use {
			Pen.translate(0,v.bounds.height/2);
			Pen.rotate(-pi/4);
			center_to_row_col = ();
			numrowscols.do({ | row |
				numrowscols.do({
					| col |

					var x = col.linlin(-1,numrowscols*sqrt(2),0,v.bounds.width);
					var y = row.linlin(-1,numrowscols*sqrt(2),0,v.bounds.width);
					var color = colorlut[(row-col)%12];
					var m11,m12,m21,m22,trx,try;
					Pen.fillColor = color;
					Pen.strokeColor = Color.black;
					a = Pen.addArc(x@y, radius, 0, 2pi);
					m = a.matrix;
					m11 = m[0];
					m21 = m[1];
					m12 = m[2];
					m22 = m[3];
					trx = m[4];
					try = m[5];
					Pen.fillStroke;

					// map from user coordinates x,y to screen coordinates using system matrix
					center_to_row_col.put( row*numrowscols + col, [(((m11*x)+(m12*y)+trx)), (((m21*x)+(m22*y)+try))]);

				});
			});
		};
		if ((lastclickedrow != nil) && (lastclickedcol != nil))
		{
			Pen.use {
				var x = lastclickedcol.linlin(-1,numrowscols*sqrt(2),0,v.bounds.width);
				var y = lastclickedrow.linlin(-1,numrowscols*sqrt(2),0,v.bounds.width);
				Pen.translate(0,v.bounds.height/2);
				Pen.rotate(-pi/4);
				Pen.strokeColor = Color.black;
				Pen.width_(5);
				Pen.addRoundedRect(Rect.aboutPoint(x@y, 2/3*radius, 2/3*radius), radius/4, radius/4);
				Pen.stroke;
			};
		};
	};
	v.mouseDownAction = {
		| view, x, y, modifiers, buttonNumber, clickCount |
		var found = False;
		center_to_row_col.keysValuesDo({
			| key, value |
			var distancesquared = (((value[0]-x)*(value[0]-x)) + ((value[1]-y)*(value[1]-y)));
			var radiussquared = radius*radius;
			if ((distancesquared < radiussquared) && (found == False))
			{
				var row = key.div(numrowscols);
				var col = key%numrowscols;
				var rownote = row.linlin(0,
					numrowscols-1,
					nametomidi.value(bottomleftnote.string),
					nametomidi.value(toprightnote.string));
				var colnote = col.linlin(0,
					numrowscols-1,
					nametomidi.value(bottomrightnote.string),
					nametomidi.value(topleftnote.string));
				lastclickedrow = row;
				lastclickedcol = col;
				found = True;
				//("row: "++row++"col: "++col).postln;
				~i1.set(\freq, rownote.midicps);
				~i2.set(\freq, colnote.midicps);
			};
		});
		if (found == False)
		{
			~i1.set(\freq, 0);
			~i2.set(\freq, 0);
			lastclickedrow = nil;
			lastclickedcol = nil;
		};

	w.refresh;

	};

	v.onClose = {
		~i1.free;
		~i2.free;
	};

	w.front;
);

});

)