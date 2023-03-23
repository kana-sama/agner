import React, { useState, useEffect, useRef } from 'react';
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
  flex-direction: column;
`;

function prettyInstr(instr) {
  function f(i) {
    if (!i) return "";
    if (typeof i === 'number') return i;
    if (typeof i === "string") return i;
    if ("arity" in i) return `${i.name}/${i.arity}`;
    return JSON.stringify(i);
  } 

  function prettyContent(i) {
    if ("contents" in i) {
      if (Array.isArray(i.contents)) {
        return i.contents.map(f).join(" ");
      } else {
        return f(i.contents);
      }
    } else {
      return Object.entries(i).filter((([k, v]) => k !== "tag")).map((([k, v]) => v)).map(f).join(" ");
    }
  }

  return `${instr.tag}\t ${prettyContent(instr)}`;
}

function Stack({ stack }) {
  return (
    <div style={{border: "1px solid black", width: "300px"}}>
      <div>Stack:</div>
      {stack.map((value, i) => (
        <div key={i} style={{borderBottom: "1px solid black"}}>{prettyValue(value)}</div>
      ))}
    </div>
  )
}

function Mem({ mem }) {
  return (
    <div style={{border: "1px", width: "300px"}}>
      <div>Mem:</div>
      {Array.from(Object.entries(mem)).map(([key, value], i) => (
        <div key={i}>{key}: {prettyValue(value)}</div>
      ))}
    </div>
  )
}

function prettyValue(value) {
  if (value.tag === "Atom") return value.contents;
  if (value.tag === "PID") return `<${value.contents}>`;
  if (value.tag === "Tuple") return `{${value.contents.map(prettyValue).join(",")}}`;
  if (value.tag === "Fun") return `fun ${value.contents.name}/${value.contents.arity}`;
  if (value.tag === "Integer") return value.contents;
  return JSON.stringify(value); 
}

function App({ prog, cfgs }) {
  const [cur, setCur] = useState(0);
  const cfg = cfgs[cur];
  const lineRef = useRef();

  useEffect(() => {
    if (lineRef.current) {
      console.log("scroll")
      // lineRef.current.scrollIntoView();
    }
  }, [cur]);
  
  return (
    <>
      <div style={{display: "flex", flexDirection: "row", position: "sticky", top: 0, background: "#eee"}}>
        <input type="range" min={0} max={cfgs.length - 1} value={cur} step={1} onChange={e => setCur(parseInt(e.target.value))}/>
        <button onClick={e => setCur(Math.max(cur - 1, 0))}>prev</button>
        {cur}
        <button onClick={e => setCur(Math.min(cur + 1, cfgs.length - 1))}>next</button>
      </div>
      <div style={{display: "flex", flexDirection: "column", border: "1px solid black",
          position: "sticky", right: 0, top: 0, left: "100%", overflow: "scroll", width: "300px",
          background: "#eee"}}>
        <div>PID: {cfg.current.pid}</div>
        <div>FUEL: {cfg.fuel}</div>
        <Stack stack={cfg.current.stack} />
        <Mem mem={cfg.current.mem} />
        <div style={{marginTop: "10px", borderTop: "1px solid black"}}>Queue</div>
        {cfg.queue.map(proc =>
          <div style={{border: "1px solid black", margin: "4px", padding: "4px"}}>
            {proc.pid}
            <Stack stack={proc.stack} />
            <Mem mem={proc.mem} />
          </div>
        )}
      </div>
      <Prog>
        {prog.map((instr, i) => (
          <ProgLine key={i} ref={lineRef} current={i === cfg.current.pos}>
            <LineNumber>{i}</LineNumber>
            <LineInstr>{prettyInstr(instr)}</LineInstr>
          </ProgLine>
        ))}
      </Prog>
    </>
  )
}

const root = ReactDOM.createRoot(document.getElementById('root'));
root.render(
  <React.StrictMode>
    <App prog={debug[0]} cfgs={debug[1]} />
  </React.StrictMode>
);
