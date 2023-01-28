import React, { useState } from 'react';
import ReactDOM from 'react-dom/client';
import styled from 'styled-components';
import debug from './debug.json';

const ProgLine = styled.div`
  display: flex;
  flex-direction: row;
  background-color: ${props => props.current ? "#222" : "#fff"};
  color: ${props => props.current ? "#fff" : "#222"};
  padding: 2px;
  font-family: monospace;
`;

const LineNumber = styled.div`width: 30px;`;
const LineInstr = styled.div``;

const Prog = styled.div`
  padding: 20px;
  display: flex;
  width: 100%;
  flex-direction: column;
`;

function prettyInstr(instr) {
  function prettyContent(i) {
    if (Array.isArray(i)) return i.map(prettyContent).join(" ");
    if (!i) return "";
    if (typeof i === 'number') return i;
    if (typeof i === "string") return i;
    if ("arity" in i) return `${i.name}/${i.arity}`
    return JSON.stringify(i);
  }

  return `${instr.tag}\t ${prettyContent(instr.contents)}`
}

function Stack({ stack }) {
  return (
    <div style={{border: "1px solid black", width: "300px"}}>
      <div>Stack:</div>
      {stack.map((value, i) => (
        <div>{JSON.stringify(value)}</div>
      ))}
    </div>
  )
}

function Mem({ mem }) {
  return (
    <div style={{border: "1px", width: "300px"}}>
      <div>Mem:</div>
      {Array.from(Object.entries(mem)).map(([key, value], i) => (
        <div>{key}: {JSON.stringify(value)}</div>
      ))}
    </div>
  )
}

function App({ prog, cfgs }) {
  const [cur, setCur] = useState(0);
  const cfg = cfgs[cur];
  
  return (
    <div style={{display: "flex", flexDirection: "column"}}>
      <div style={{display: "flex", flexDirection: "row"}}>
        <input type="range" min={0} max={cfgs.length - 1} value={cur} step={1} onChange={e => setCur(e.target.value)}/>
        <button onClick={e => setCur(Math.max(cur - 1, 0))}>prev</button>
        {cur}
        <button onClick={e => setCur(Math.min(cur + 1, cfgs.length - 1))}>next</button>
      </div>
      <div style={{display: "flex", flexDirection: "row"}}>
        <Prog>
          {prog.map((instr, i) => (
            <ProgLine key={i} current={i == cfg.pos}>
              <LineNumber>{i}</LineNumber>
              <LineInstr>{prettyInstr(instr)}</LineInstr>
            </ProgLine>
          ))}
        </Prog>
        <div style={{display: "flex", flexDirection: "column", border: "1px solid black"}}>
          <Stack stack={cfg.stack} />
          <Mem mem={cfg.mem} />

          <div style={{display: "flex", flexDirection: "column", border: "1px solid black"}}>
            {cfg.frames.map(frame => (
              <div style={{display: "flex", flexDirection: "column", border: "1px solid black"}}>
                <Stack stack={frame.stack} />
                <Mem mem={frame.mem} />
              </div>
            ))}
          </div>
        </div>
      </div>
    </div>
  )
}

const root = ReactDOM.createRoot(document.getElementById('root'));
root.render(
  <React.StrictMode>
    <App prog={debug.prog} cfgs={debug.cfgs} />
  </React.StrictMode>
);
