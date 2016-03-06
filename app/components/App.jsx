import React from 'react';
import PlotSelector from './PlotSelector.jsx';

export default class App extends React.Component {
  render() {
    return (
      <div className='w3-container w3-row'>
        <PlotSelector addPlot={this.addPlot.bind(this)} />
      </div>
    );
  }

  // Callback from the PlotSelector with a link to the selected plot.
  addPlot(link) {
    console.log("Got link: " + link);
  }
}
