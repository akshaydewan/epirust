/*
 * EpiRust
 * Copyright (c) 2020  ThoughtWorks, Inc.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

import React from "react";
import './job.scss'
import PropTypes from "prop-types";
import {Link} from "react-router-dom";

export const Job = ({simulationId, status}) => {
  const statusText = {
    "running": "In-Progress",
    "finished": "Finished",
    "failed": "Failed",
    "in-queue": "In-Queue"
  };

  return (
    <li className="list-group-item borderless item-less-padding">
      <Link to={`/jobs/${simulationId}`} className={'no-link-formatting'}>
        <div className={`card ${status}`}>
          <div className="card-body card-body-less-padding">
            <div className="job-status">{statusText[status]}</div>
            <div className="minor-details">Simulation Id: {simulationId} </div>
          </div>
        </div>
      </Link>
    </li>
  );
};

Job.propTypes = {
  simulationId: PropTypes.number.isRequired,
  status: PropTypes.oneOf(["running", "finished", "failed", "in-queue"]).isRequired
};