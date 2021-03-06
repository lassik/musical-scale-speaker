"use strict";

var app;

function speakString(str) {
  var utterance = new SpeechSynthesisUtterance(str);
  utterance.rate = 2;
  utterance.pitch = 1;
  utterance.volume = 0.3;
  speechSynthesis.cancel();
  speechSynthesis.speak(utterance);
}

function stringFromBytes(bytes) {
  return String.fromCharCode.apply(String, bytes);
}

function onMidiMessage(event) {
  app.ports.onMidiMessage.send({ midiEvent: stringFromBytes(event.data) });
}

function initMidiPort(port) {
  if (port.type === "input" && port.state === "connected") {
    console.log("MIDI input connected:", port);
    port.onmidimessage = onMidiMessage;
  }
}

function onMidiStateChange(event) {
  initMidiPort(event.port);
}

function initMidi(midi) {
  midi.inputs.forEach(initMidiPort);
  midi.onstatechange = onMidiStateChange;
}

if (navigator.requestMIDIAccess) {
  navigator.requestMIDIAccess().then(initMidi, console.log);
}

app = Elm.Main.init({
  node: document.getElementById("elm")
});

app.ports.speak.subscribe(speakString);
