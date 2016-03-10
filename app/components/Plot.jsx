import React from 'react';
import JQuery from 'jquery';

const PlotHeight = 550;
const PlotWidth = 750;

export default class Plot extends React.Component {

  constructor(props) {
    super(props);

    // Instance variable to give a unique name for the div to host
    // the Plotly chart.
    this.plotId = 'plot' + props.plot.seq.toString();

    // Instance variable to hold the entry to display.
    this.entry = props.plot.entry;

    // Instance variable to hold websocket. Shall be created in
    // componentDidMount?
    this.ws = null;

    // Set the state's plot to an "empty" plot.
    this.state = {
      data: [],
      layout: {
        height: PlotHeight,
        width: PlotWidth,
        title: this.entry.description
      }
    };
  }

  // Initial rendering of Plotly stuff.
  componentDidMount() {
    console.log('mount');
    //var data=[{
    //  values: [19, 26, 77],
    //  labels: ['beer', 'pizza', 'snacks'],
    //  type: 'pie'
    //}];
    //var data=[];
    //var layout={
      //title: 'Just a dummy chart',
      //height: 550,
      //width: 750
    //};

    // Render the initial plot using default data.
    Plotly.newPlot(this.plotId, this.state.data, this.state.layout);
    console.log('Now fetching from: ' + this.entry.link);

    // Requesting live data from server using REST.
    JQuery.getJSON(this.entry.link, data => {
      console.log("Got something, try update state");
      console.log("h: " + PlotHeight + " w: " + PlotWidth);
      data.layout.height = PlotHeight;
      data.layout.width = PlotWidth;
      data.layout.title = this.entry.description;
      this.setState(data);
    });

    const wsUrl = this.mkWsEndpoint(this.entry.link);
    console.log('WebSocket url to use: ' + wsUrl);
    this.ws = new WebSocket(wsUrl);
    this.ws.onopen = this.handleWsOpen;
    this.ws.onclose = this.handleWsClose;
    this.ws.onmessage = this.handleWsMessage.bind(this);
  }

  // Update the Plotly stuff with new data and layout.
  componentDidUpdate() {
    console.log("componentDidUpdate()");
    Plotly.newPlot(this.plotId, this.state.data, this.state.layout);
  }

  render() {
    return (
        <div className='w3-card-12 w3-margin' style={{width:800}}>
          <header className='w3-container w3-blue w3-right-align'>
            <span onClick={this.handleClose.bind(this, this.props.plot.seq)}
                  className="clickable">
               ✖
            </span>
          </header>
          <div id={this.plotId}
               className='w3-container' style={{height:600}}>
          </div>
        </div>
    );
  }

  // Event handler when the 'cross' has been clicked.
  handleClose(seq) {
    console.log('handleClose: ' + seq);
    // Propagate the event to the PlotGrid.
    this.props.removePlot(seq);
  }

  handleWsOpen() {
    console.log('WebSocket did open');
  }

  handleWsClose() {
    console.log('WebSocket did close');
  }

  handleWsMessage(evt) {
    console.log('WebSocket got: ' + evt.data);
  }

  mkWsEndpoint(url) {
    return 'ws://localhost:8888' + url;
  }
}
