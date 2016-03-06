import React from 'react';

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
        <table className='w3-table w3-striped'>
          <thead>
            <tr className='w3-blue'>
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
        <button className='w3-btn-block w3-light-grey'
                onClick={this.handleUpdate.bind(this)}>Update plot list
        </button>
      </div>);
  }

  // Update the state with a new plot list from the server.
  handleUpdate() {
    console.log('Update clicked');
    this.setState({plots: [
      {description: 'Dummy plot no 1', type: 'pie', link: '/foo/123'},
      {description: 'Dummy plot no 2', type: 'pie', link: '/foo/456'},
      {description: 'Dummy plot no 3', type: 'chart', link: '/foo/789'}
    ]});
  }

  // Handle a new link is selected for adding.
  handleAdd(entry) {
    console.log("handleAdd: " + entry.link);
    this.props.addPlot(entry);
  }
}
