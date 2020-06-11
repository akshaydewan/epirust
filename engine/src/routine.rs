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

use crate::agent::{Citizen, WorkerType};
use crate::constants::HOURS_IN_A_DAY;
use crate::routine::RoutineTask::{AtHome, Sleeping, UsingPublicTransport, Working, RoamingInHousingArea};

/// What routine an agent is following
#[derive(Copy, Clone, Debug, Serialize, Deserialize, PartialEq)]
pub enum Routine {
    Normal,
    HomeIsolated,
    WorkQuarantinedAsHealthcareWorker,
    Hospitalized,
    Dead,
}

/// Union of all possible tasks in all routines
#[derive(Copy, Clone, Debug, Serialize, Deserialize, PartialEq)]
pub enum RoutineTask {
    Sleeping,
    UsingPublicTransport,
    Working,
    RoamingInHousingArea,
    AtHome,
    WorkQuarantinedAsHealthcareWorker,
    Hospitalized,
    Dead,
}

pub fn get_routine_task(citizen: &Citizen, simulation_hour: i32, lockdown: bool) -> RoutineTask {
    match citizen.get_current_routine(lockdown) {
        Routine::HomeIsolated => {
            RoutineTask::AtHome
        }
        Routine::WorkQuarantinedAsHealthcareWorker => {
            RoutineTask::WorkQuarantinedAsHealthcareWorker
        }
        Routine::Hospitalized => {
            RoutineTask::Hospitalized
        }
        Routine::Normal => {
            get_normal_routine(citizen, simulation_hour)
        }
        Routine::Dead => {
            RoutineTask::Dead
        }
    }
}

fn get_normal_routine(citizen: &Citizen, simulation_hour: i32) -> RoutineTask {
    match citizen.get_worker_type() {
        WorkerType::Normal => {
            healthy_worker_routine(citizen, simulation_hour)
        }
        WorkerType::Essential => {
            healthy_worker_routine(citizen, simulation_hour)
        }
        WorkerType::HospitalStaff { .. } => {
            // healthy_healthcare_worker_routine(citizen, simulation_hour) TODO
            healthy_worker_routine(citizen, simulation_hour)
        }
        WorkerType::NA => {
            healthy_non_worker_routine(simulation_hour)
        }
    }
}

fn healthy_worker_routine(citizen: &Citizen, simulation_hour: i32) -> RoutineTask {
    let hour_of_day = simulation_hour % HOURS_IN_A_DAY;
    match hour_of_day {
        0..=6 => {
            Sleeping
        }
        7 => {
            if citizen.uses_public_transport {
                UsingPublicTransport
            } else {
                AtHome
            }
        }
        8..=16 => {
            Working
        }
        17 => {
            if citizen.uses_public_transport {
                UsingPublicTransport
            } else {
                AtHome
            }
        }
        _ => {
            AtHome
        }
    }
}

fn healthy_non_worker_routine(simulation_hour: i32) -> RoutineTask {
    let hour_of_day = simulation_hour % HOURS_IN_A_DAY;
    match hour_of_day {
        0..=6 => {
            Sleeping
        }
        7 => {
            AtHome
        }
        8..=16 => {
            RoamingInHousingArea
        }
        _ => {
            AtHome
        }
    }
}

// fn healthy_healthcare_worker_routine(simulation_hour: i32) -> Option<Area> {
//
// }

#[cfg(test)]
mod tests {
    use super::*;
    use crate::geography::{Area, Point};

    #[test]
    fn should_get_routine_tasks_for_healthy_worker() {
        let citizen = crate::agent::mother::working_citizen();

        let mut expected_tasks = vec![];
        for _i in 0..=6 {
            expected_tasks.push(Sleeping);
        }
        expected_tasks.push(AtHome);
        for _i in 8..=16 {
            expected_tasks.push(Working);
        }
        for _i in 17..=23 {
            expected_tasks.push(AtHome);
        }

        let mut actual_tasks = vec![];
        for hr in 0..24 {
            actual_tasks.push(super::get_routine_task(&citizen, hr, false));
        }

        assert_eq!(expected_tasks, actual_tasks);
    }

    #[test]
    fn should_get_routine_tasks_for_healthy_worker_using_pub_transport() {
        let mut citizen = crate::agent::mother::working_citizen();
        citizen.uses_public_transport = true;

        let mut expected_tasks = vec![];
        for _i in 0..=6 {
            expected_tasks.push(Sleeping);
        }
        expected_tasks.push(UsingPublicTransport);
        for _i in 8..=16 {
            expected_tasks.push(Working);
        }
        expected_tasks.push(UsingPublicTransport);
        for _i in 18..=23 {
            expected_tasks.push(AtHome);
        }

        let mut actual_tasks = vec![];
        for hr in 0..24 {
            actual_tasks.push(super::get_routine_task(&citizen, hr, false));
        }

        assert_eq!(expected_tasks, actual_tasks);
    }

    #[test]
    fn should_get_routine_tasks_for_healthy_non_worker() {
        let citizen = crate::agent::mother::non_working_citizen();

        let mut expected_tasks = vec![];
        for _i in 0..=6 {
            expected_tasks.push(Sleeping);
        }
        expected_tasks.push(AtHome);
        for _i in 8..=16 {
            expected_tasks.push(RoamingInHousingArea);
        }
        for _i in 17..24 {
            expected_tasks.push(AtHome);
        }

        let mut actual_tasks = vec![];
        for hr in 0..24 {
            actual_tasks.push(super::get_routine_task(&citizen, hr, false));
        }

        assert_eq!(expected_tasks, actual_tasks);
    }

    #[test]
    fn should_get_routine_tasks_for_hospitalized_agent() {
        let hospital_area = Area::new(Point::new(0, 0), Point::new(5, 5));
        let citizen = crate::agent::mother::hospitalized_citizen(hospital_area);
        for hr in 0..24 {
            let task = super::get_routine_task(&citizen, hr, false);
            assert_eq!(task, RoutineTask::Hospitalized);
        }
    }

    #[test]
    fn should_get_routine_tasks_for_dead_agent() {
        let citizen = crate::agent::mother::dead_citizen();
        for hr in 0..24 {
            let task = super::get_routine_task(&citizen, hr, false);
            assert_eq!(task, RoutineTask::Dead);
        }
    }

    #[test]
    fn should_get_routine_tasks_for_home_isolated_agent() {
        let mut citizen = crate::agent::mother::home_isolated_citizen();
        for hr in 0..24 {
            let task = super::get_routine_task(&citizen, hr, false);
            assert_eq!(task, RoutineTask::AtHome);
        }
    }

    #[test]
    fn should_get_routine_tasks_for_healthcare_worker() {
        should_get_routine_tasks_for_healthy_worker();
    }

}
