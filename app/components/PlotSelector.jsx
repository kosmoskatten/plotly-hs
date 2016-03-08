import React from 'react';
import JQuery from 'jquery';

/*
 * The PlotSelector displays a list of available plots and let the user
 * add one of those for display.
 */
export default class PlotSelector extends React.Component {
  constructor(props) {
    super(props);
    this.state = {plots: []};
  }

  componentDidMount() {
    // Force an update as soon as the selector has been mounted.
    this.handleUpdate();
  }

  render() {
    // Render the PlotSelector.
    return (
      <div className='w3-container w3-col m4 l4'>
        <div className='w3-card w3-margin'>
        <header className='w3-container w3-blue'>
          <h3>Select your plot</h3>
        </header>

        <table className='w3-table w3-striped'>
          <thead>
            <tr className='w3-light-blue'>
              <th>Plot Description</th>
              <th>Type</th>
              <th>Add It!</th>
            </tr>
          </thead>
          <tbody>

            { /* Map all plot descriptors to table rows. */ }
            {this.state.plots.map((entry, i) => {
              return (
                <tr key={i}>
                  <td>{entry.description}</td>
                  <td>{entry.type}</td>
                  <td>
                    <a className='w3-btn-floating w3-blue'
                       onClick={this.handleAdd.bind(this, entry)}>+</a>
                  </td>
                </tr>
              );
            })}

        </tbody>
        </table>
        <button className='w3-btn-block w3-dark-grey'
                onClick={this.handleUpdate.bind(this)}>Update plot list
        </button>

        </div>
      </div>
      );
  }

  // Update the state with a new plot list from the server.
  handleUpdate() {
    console.log('Update clicked');
    JQuery.getJSON('/rest/plot', data => {
      console.log('Got something');
      this.setState({plots: data});
    });
    /*this.setState({plots: [
      {description: 'Dummy plot no 1', type: 'pie', link: '/foo/123'},
      {description: 'Dummy plot no 2', type: 'pie', link: '/foo/456'},
      {description: 'Dummy plot no 3', type: 'chart', link: '/foo/789'}
    ]});*/
  }

  // Handle a new link is selected for adding.
  handleAdd(entry) {
    console.log("handleAdd: " + entry.link);
    this.props.addPlot(entry);
  }
}
