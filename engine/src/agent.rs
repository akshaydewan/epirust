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

use rand::Rng;
use rand::seq::IteratorRandom;
use rand::seq::SliceRandom;
use serde::{de, Deserialize, Deserializer};
use serde::de::Unexpected;
use uuid::Uuid;

use crate::allocation_map::AgentLocationMap;
use crate::config::StartingInfections;
use crate::constants;
use crate::disease::Disease;
use crate::disease_state_machine::{DiseaseStateMachine, State};
use crate::geography::{Area, Grid, Point};
use crate::random_wrapper::RandomWrapper;
use crate::travel_plan::Traveller;
use crate::routine::{RoutineTask, Routine};
use crate::routine::Routine::{HomeIsolated, Normal};
use crate::routine::RoutineTask::Working;

#[derive(Deserialize)]
pub struct PopulationRecord {
    //TODO move to a better place
    pub ind: i32,
    pub age: String,
    #[serde(deserialize_with = "bool_from_string")]
    pub working: bool,
    #[serde(deserialize_with = "bool_from_string")]
    pub pub_transport: bool,
}

/// Deserialize bool from String with custom value mapping
fn bool_from_string<'de, D>(deserializer: D) -> Result<bool, D::Error>
    where
        D: Deserializer<'de>,
{
    match String::deserialize(deserializer)?.as_ref() {
        "True" => Ok(true),
        "False" => Ok(false),
        other => Err(de::Error::invalid_value(
            Unexpected::Str(other),
            &"True or False",
        )),
    }
}

#[derive(Copy, Clone, Debug, Serialize, Deserialize, PartialEq)]
pub enum WorkerType {
    Normal,
    Essential,
    HospitalStaff { work_start_at: i32 },
    NA,
}

#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
pub struct Citizen {
    pub id: Uuid,
    immunity: i32,
    pub home_location: Area,
    pub work_location: Area,
    vaccinated: bool,
    pub uses_public_transport: bool,
    working: bool,
    pub transport_location: Point,
    pub state_machine: DiseaseStateMachine,
    current_area: Area,
    worker_type: WorkerType,
    current_routine: Routine,
}

impl Citizen {
    pub fn new(home_location: Area, work_location: Area, transport_location: Point,
               uses_public_transport: bool, working: bool, work_status: WorkerType, rng: &mut RandomWrapper) -> Citizen {
        Citizen::new_with_id(Uuid::new_v4(), home_location, work_location, transport_location, uses_public_transport,
                             working, work_status, rng)
    }

    pub fn new_with_id(id: Uuid, home_location: Area, work_location: Area, transport_location: Point,
                       uses_public_transport: bool, working: bool, work_status: WorkerType, rng: &mut RandomWrapper) -> Citizen {
        let disease_randomness_factor = Citizen::generate_disease_randomness_factor(rng);

        Citizen {
            id,
            immunity: disease_randomness_factor,
            home_location,
            work_location,
            transport_location,
            vaccinated: false,
            uses_public_transport,
            working,
            state_machine: DiseaseStateMachine::new(),
            current_area: home_location,
            worker_type: work_status,
            current_routine: Routine::Normal,
        }
    }

    pub fn from_traveller(traveller: &Traveller, home_location: Area, work_location: Area,
                          transport_location: Point, current_area: Area) -> Citizen {
        Citizen {
            id: traveller.id,
            immunity: traveller.immunity,
            home_location,
            work_location,
            vaccinated: traveller.vaccinated,
            uses_public_transport: traveller.uses_public_transport,
            working: false,
            transport_location,
            state_machine: traveller.state_machine,
            current_area,
            worker_type: WorkerType::NA {},
            current_routine: Routine::Normal,
        }
    }

    pub fn from_record(record: PopulationRecord, home_location: Area, work_location: Area,
                       transport_location: Point, rng: &mut RandomWrapper) -> Citizen {
        let disease_randomness_factor = Citizen::generate_disease_randomness_factor(rng);
        let work_status = Citizen::derive_worker_type(record.working, rng);

        Citizen {
            id: Uuid::new_v4(),
            immunity: disease_randomness_factor,
            home_location,
            work_location,
            transport_location,
            vaccinated: false,
            uses_public_transport: record.pub_transport,
            working: record.working,
            state_machine: DiseaseStateMachine::new(),
            current_area: home_location,
            worker_type: work_status,
            current_routine: Routine::Normal,
        }
    }

    fn generate_disease_randomness_factor(rng: &mut RandomWrapper) -> i32 {
        let option = constants::IMMUNITY_RANGE.choose(rng.get());
        *option.unwrap()
    }

    pub fn receive_tick(&mut self, current_location: Point, simulation_hour: i32, grid: &Grid, map: &AgentLocationMap,
                        rng: &mut RandomWrapper, disease: &Disease, lockdown: bool) -> Point {
        let hour_of_day = simulation_hour % constants::NUMBER_OF_HOURS;
        if hour_of_day == constants::ROUTINE_START_TIME {
            self.started_new_day(map, grid, disease, rng);
        }
        let routine_task = crate::routine::get_routine_task(self, simulation_hour, lockdown);
        let new_destination = self.new_destination(routine_task, grid);
        return if new_destination.is_some() {
            let new_destination = new_destination.unwrap();
            let new_point = new_destination.get_random_point(rng);
            if map.is_cell_vacant(&new_point) {
                self.current_area = new_destination;
                self.update_disease_progression(new_point, map, simulation_hour, rng, disease);
                new_point
            } else {
                current_location
            }
        } else {
            current_location
        };
    }

    fn new_destination(&self, routine_task: RoutineTask, grid: &Grid) -> Option<Area> {
        match routine_task {
            RoutineTask::Sleeping => {
                if self.current_area == self.home_location {
                    None
                } else {
                    Some(self.home_location)
                }
            }
            RoutineTask::UsingPublicTransport => {
                Some(grid.transport_area)
            }
            RoutineTask::Working => {
                match self.worker_type {
                    WorkerType::HospitalStaff { .. } => { Some(grid.hospital_area) }
                    _ => { Some(self.work_location) }
                }
            }
            RoutineTask::RoamingInHousingArea => {
                Some(grid.housing_area)
            }
            RoutineTask::AtHome => {
                Some(self.home_location)
            }
            RoutineTask::WorkQuarantinedAsHealthcareWorker => {
                Some(grid.hospital_area) //TODO
            }
            RoutineTask::Hospitalized => {
                if self.current_area == grid.hospital_area {
                    None
                } else {
                    Some(grid.hospital_area)
                }
            }
            RoutineTask::Dead => {
                if self.current_area == self.home_location {
                    None
                } else {
                    return Some(self.home_location);
                }
            }
        }
    }

    fn started_new_day(&mut self, map: &AgentLocationMap, grid: &Grid, disease: &Disease,
                       rng: &mut RandomWrapper) {
        self.state_machine.started_new_day();
        self.check_and_hospitalize(map, grid, disease);
        self.check_and_die(rng, disease);
    }

    fn update_infection_severity(&mut self, sim_hr: i32, rng: &mut RandomWrapper, disease: &Disease) {
        if self.state_machine.is_pre_symptomatic() {
            self.state_machine.change_infection_severity(sim_hr, rng, disease);
        }
    }

    fn update_disease_progression(&mut self, cell: Point, map: &AgentLocationMap, sim_hr: i32,
                                  rng: &mut RandomWrapper, disease: &Disease) {
        self.update_exposure(cell, map, sim_hr, rng, disease);
        self.update_infection(sim_hr, rng, &disease);
        self.update_infection_severity(sim_hr, rng, disease);
    }

    fn update_infection(&mut self, sim_hr: i32, rng: &mut RandomWrapper, disease: &Disease) {
        if self.state_machine.is_exposed() {
            self.state_machine.infect(rng, sim_hr, &disease);
        }
    }

    fn check_and_hospitalize(&mut self, map: &AgentLocationMap, grid: &Grid, disease: &Disease) {
        if self.state_machine.is_infected() && self.is_hospitalized() {
            let to_be_hospitalized = self.state_machine.hospitalize(disease, self.immunity);
            if !to_be_hospitalized {
                return;
            }
            let hospital_has_space = map.hospital_has_space(&grid.hospital_area);
            if hospital_has_space {
                self.current_routine = Routine::Hospitalized;
            } else {
                self.current_routine = Routine::HomeIsolated;
            }
        }
    }

    fn check_and_die(&mut self, rng: &mut RandomWrapper, disease: &Disease) {
        if self.state_machine.is_infected() {
            let result = self.state_machine.decease_or_recover(rng, disease);
            match result {
                State::Recovered { .. } => {
                    self.current_routine = Routine::Normal
                }
                State::Deceased { .. } => {
                    self.current_routine = Routine::Dead
                }
                _ => {}
            }
        }
    }

    fn update_exposure(&mut self, cell: Point, map: &AgentLocationMap, sim_hr: i32,
                       rng: &mut RandomWrapper, disease: &Disease) {
        if self.can_catch_infection() {
            let neighbour_cells = self.current_area.get_neighbors_of(cell);

            let neighbor_that_spreads_infection = neighbour_cells
                .filter(|p| map.is_point_in_grid(p))
                .filter_map(|cell| { map.get_agent_for(&cell) })
                .filter(|agent| agent.state_machine.is_infected() && !agent.is_hospitalized())
                .find(|neighbor| rng.get().gen_bool(neighbor.get_infection_transmission_rate(disease)));

            if neighbor_that_spreads_infection.is_some() {
                self.state_machine.expose(sim_hr);
            }
        }
    }

    fn can_catch_infection(&self) -> bool {
        self.state_machine.is_susceptible() && !self.quarantined_as_healthcare_worker()
            && !self.vaccinated
    }

    pub fn is_hospitalized(&self) -> bool {
        self.current_routine == Routine::Hospitalized
    }

    fn quarantined_as_healthcare_worker(&self) -> bool {
        self.current_routine == Routine::WorkQuarantinedAsHealthcareWorker
    }

    pub fn get_worker_type(&self) -> WorkerType {
        self.worker_type
    }

    pub fn is_working(&self) -> bool {
        self.working
    }

    pub fn get_immunity(&self) -> i32 {
        self.immunity
    }

    pub fn is_vaccinated(&self) -> bool {
        self.vaccinated
    }

    pub fn get_current_routine(&self, lockdown: bool) -> Routine {
        if !lockdown {
            self.current_routine
        } else {
            match self.current_routine {
                Normal => {
                    if self.allowed_to_work_during_lockdown() {
                        self.current_routine
                    } else {
                        Routine::HomeIsolated
                    }
                }
                HomeIsolated => { self.current_routine }
                Routine::WorkQuarantinedAsHealthcareWorker => { self.current_routine }
                Routine::Hospitalized => { self.current_routine }
                Routine::Dead => { self.current_routine }
            }
        }
    }

    pub fn allowed_to_work_during_lockdown(&self) -> bool {
        match self.worker_type {
            WorkerType::Normal => { false }
            WorkerType::Essential => { true }
            WorkerType::HospitalStaff { .. } => { true }
            WorkerType::NA => { false }
        }
    }

    pub fn get_infection_transmission_rate(&self, disease: &Disease) -> f64 {
        disease.get_current_transmission_rate(self.state_machine.get_infection_day() + self.immunity)
    }

    fn derive_worker_type(is_working: bool, rng: &mut RandomWrapper) -> WorkerType {
        if is_working {
            if rng.get().gen_bool(constants::HOSPITAL_STAFF_PERCENTAGE) {
                return WorkerType::HospitalStaff { work_start_at: constants::ROUTINE_WORK_TIME };
            }
            return WorkerType::Normal;
        }
        return WorkerType::NA;
    }

    pub fn assign_essential_worker(&mut self, essential_workers_percentage: f64, rng: &mut RandomWrapper) {
        match self.worker_type {
            WorkerType::Normal {} => {
                if rng.get().gen_bool(essential_workers_percentage) {
                    self.worker_type = WorkerType::Essential;
                }
            }
            _ => {}
        }
    }

    pub fn set_vaccination(&mut self, vaccinated: bool) {
        self.vaccinated = vaccinated;
    }

    pub fn can_travel(&self, lockdown: bool) -> bool {
        self.get_current_routine(lockdown) == Routine::Normal
    }

    pub fn is_essential_worker(&self) -> bool {
        self.worker_type == WorkerType::Essential
    }

    #[cfg(test)]
    pub fn is_exposed(&self) -> bool {
        self.state_machine.is_exposed()
    }

    #[cfg(test)]
    pub fn is_mild_asymptomatic(&self) -> bool {
        self.state_machine.is_mild_asymptomatic()
    }

    #[cfg(test)]
    pub fn is_mild_symptomatic(&self) -> bool {
        self.state_machine.is_mild_symptomatic()
    }

    #[cfg(test)]
    pub fn is_infected_severe(&self) -> bool {
        self.state_machine.is_infected_severe()
    }
}

pub fn citizen_factory(number_of_agents: i32, home_locations: &Vec<Area>, work_locations: &Vec<Area>, public_transport_locations: &Vec<Point>,
                       percentage_public_transport: f64, working_percentage: f64, rng: &mut RandomWrapper,
                       starting_infections: &StartingInfections) -> Vec<Citizen> {
    let mut agent_list = Vec::with_capacity(home_locations.len());

    for i in 0..number_of_agents as usize {
        let is_a_working_citizen = rng.get().gen_bool(working_percentage);

        let total_home_locations = home_locations.len();
        let total_work_locations = work_locations.len();

        let home_location = home_locations[(i % total_home_locations)];
        let work_location = work_locations[(i % total_work_locations)];

        let uses_public_transport = rng.get().gen_bool(percentage_public_transport)
            && is_a_working_citizen
            && i < public_transport_locations.len();
        //TODO: Check the logic - Jayanta
        let public_transport_location: Point = if uses_public_transport { public_transport_locations[i] } else {
            home_location.get_random_point(rng)
        };

        let work_location = if is_a_working_citizen { work_location } else {
            home_location
        };
        let work_status = Citizen::derive_worker_type(is_a_working_citizen, rng);

        let agent = Citizen::new(home_location, work_location, public_transport_location,
                                 uses_public_transport, is_a_working_citizen, work_status, rng);

        agent_list.push(agent);
    }

    set_starting_infections(&mut agent_list, starting_infections, rng);

    agent_list
}

pub fn set_starting_infections(agent_list: &mut Vec<Citizen>, start_infections: &StartingInfections,
                               rng: &mut RandomWrapper) {
    if start_infections.total() as usize > agent_list.len() {
        panic!("There are {} people set to infect, but only {} agents available",
               start_infections.total(), agent_list.len())
    }
    if start_infections.total() == 0 {
        warn!("Simulation configured to start without any infected agents");
    }
    let mut to_infect = agent_list.iter_mut().choose_multiple(rng.get(), start_infections.total() as usize);
    let mut citizens = to_infect.iter_mut();

    for _i in 0..start_infections.get_exposed() {
        citizens.next().unwrap().state_machine.expose(0);
    }
    for _i in 0..start_infections.get_infected_mild_asymptomatic() {
        citizens.next().unwrap().state_machine.set_mild_asymptomatic()
    }
    for _i in 0..start_infections.get_infected_mild_symptomatic() {
        citizens.next().unwrap().state_machine.set_mild_symptomatic()
    }
    for _i in 0..start_infections.get_infected_severe() {
        citizens.next().unwrap().state_machine.set_severe_infected()
    }
}

#[cfg(test)]
pub mod mother {
    use super::*;

    pub fn working_citizen() -> Citizen {
        let home_location = Area::new(Point::new(0, 0), Point::new(10, 10));
        let work_location = Area::new(Point::new(11, 0), Point::new(20, 20));
        Citizen::new(home_location, work_location, Point::new(2, 2), false, true, WorkerType::Normal,
                     &mut RandomWrapper::new())
    }

    pub fn non_working_citizen() -> Citizen {
        let home_location = Area::new(Point::new(0, 0), Point::new(10, 10));
        Citizen::new(home_location, home_location, Point::new(2, 2), false, true, WorkerType::NA,
                     &mut RandomWrapper::new())
    }

    pub fn hospitalized_citizen(hospital_area: Area) -> Citizen {
        let mut citizen = working_citizen();
        citizen.current_routine = Routine::Hospitalized;
        citizen.current_area = hospital_area;
        citizen
    }

    pub fn dead_citizen() -> Citizen {
        let mut citizen = working_citizen();
        let state_machine = crate::disease_state_machine::mother::deceased();
        citizen.state_machine = state_machine;
        citizen.current_routine = Routine::Dead;
        citizen
    }

    pub fn home_isolated_citizen() -> Citizen {
        let mut citizen = working_citizen();
        citizen.current_routine = Routine::HomeIsolated;
        citizen
    }

    pub fn healthcare_worker() -> Citizen {
        let mut citizen = working_citizen();
        citizen.worker_type = WorkerType::HospitalStaff { work_start_at: 8 };
        citizen
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::geography::define_geography;

    fn before_each() -> Vec<Citizen> {
        let mut rng = RandomWrapper::new();
        let home_locations = vec![Area::new(Point::new(0, 0), Point::new(2, 2)), Area::new(Point::new(3, 0), Point::new(4, 2))];

        let work_locations = vec![Area::new(Point::new(5, 0), Point::new(6, 2)), Area::new(Point::new(7, 0), Point::new(8, 2))];

        let public_transport_location = vec![Point::new(5, 0), Point::new(5, 1), Point::new(5, 2), Point::new(5, 3)];
        let start_infections = StartingInfections::new(0, 0, 0, 1);
        citizen_factory(4, &home_locations, &work_locations, &public_transport_location, 0.5, 0.5,
                        &mut rng, &start_infections)
    }

    #[test]
    fn generate_citizen() {
        let citizen_list = before_each();
        let expected_home_locations = vec![Area::new(Point::new(0, 0), Point::new(2, 2)), Area::new(Point::new(3, 0), Point::new(4, 2))];

        assert_eq!(citizen_list.len(), 4);
        assert_eq!(citizen_list.iter().filter(|c| c.is_exposed()).count(), 1);

        for citizen in &citizen_list {
            assert!(expected_home_locations.contains(&citizen.home_location));
        }
    }

    #[test]
    fn should_set_starting_infections() {
        let home_location = Area::new(Point::new(0, 0), Point::new(10, 10));
        let work_location = Area::new(Point::new(11, 0), Point::new(20, 20));
        let mut citizens = Vec::new();
        let mut rng = RandomWrapper::new();
        for _i in 0..20 {
            let citizen = Citizen::new(home_location, work_location, Point::new(2, 2), false,
                                       true, WorkerType::Normal, &mut rng);
            citizens.push(citizen);
        }

        let start_infections = StartingInfections::new(2, 3, 4, 5);

        set_starting_infections(&mut citizens, &start_infections, &mut rng);

        let actual_exposed = citizens.iter().filter(|citizen| citizen.is_exposed()).count();
        let actual_mild_asymp = citizens.iter().filter(|citizen| citizen.is_mild_asymptomatic()).count();
        let actual_mild_symp = citizens.iter().filter(|citizen| citizen.is_mild_symptomatic()).count();
        let actual_severe = citizens.iter().filter(|citizen| citizen.is_infected_severe()).count();

        assert_eq!(2, actual_mild_asymp);
        assert_eq!(3, actual_mild_symp);
        assert_eq!(4, actual_severe);
        assert_eq!(5, actual_exposed);
    }

    #[test]
    fn should_move_to_home_location_when_sleeping() {
        let mut citizen = mother::working_citizen();
        citizen.current_area = citizen.work_location;
        let grid = define_geography(50);
        let new_destination = citizen.new_destination(RoutineTask::Sleeping, &grid);
        assert!(new_destination.is_some());
        assert_eq!(new_destination.unwrap(), citizen.home_location);
    }

    #[test]
    fn should_not_move_to_home_location_when_already_there() {
        let mut citizen = mother::working_citizen();
        citizen.current_area = citizen.home_location;
        let grid = define_geography(50);
        let new_destination = citizen.new_destination(RoutineTask::Sleeping, &grid);
        assert!(new_destination.is_none());
    }

    #[test]
    fn should_move_to_public_transport_area() {
        let mut citizen = mother::working_citizen();
        citizen.uses_public_transport = true;
        let grid = define_geography(50);
        let new_destination = citizen.new_destination(RoutineTask::UsingPublicTransport, &grid);
        assert!(new_destination.is_some());
        assert_eq!(new_destination.unwrap(), grid.transport_area);
    }

    #[test]
    fn should_move_to_housing_area() {
        let mut citizen = mother::non_working_citizen();
        let grid = define_geography(50);
        let new_destination = citizen.new_destination(RoutineTask::RoamingInHousingArea, &grid);
        assert!(new_destination.is_some());
        assert_eq!(new_destination.unwrap(), grid.housing_area);
    }

    #[test]
    fn should_move_in_home() {
        let citizen = mother::non_working_citizen();
        let grid = define_geography(50);
        let new_destination = citizen.new_destination(RoutineTask::AtHome, &grid);
        assert!(new_destination.is_some());
        assert_eq!(new_destination.unwrap(), citizen.home_location);
    }

    #[test]
    fn should_move_to_hospital_when_hospitalized() {
        let citizen = mother::working_citizen();
        let grid = define_geography(50);
        let new_destination = citizen.new_destination(RoutineTask::Hospitalized, &grid);
        assert!(new_destination.is_some());
        assert_eq!(new_destination.unwrap(), grid.hospital_area);
    }

    #[test]
    fn should_not_move_when_already_hospitalized() {
        let grid = define_geography(50);
        let citizen = mother::hospitalized_citizen(grid.hospital_area);
        let new_destination = citizen.new_destination(RoutineTask::Hospitalized, &grid);
        assert!(new_destination.is_none());
    }

    #[test]
    fn should_move_to_home_when_dead() {
        let grid = define_geography(50);
        let mut citizen = mother::dead_citizen();
        citizen.current_area = grid.hospital_area;
        let new_destination = citizen.new_destination(RoutineTask::Dead, &grid);
        assert!(new_destination.is_some());
        assert_eq!(new_destination.unwrap(), citizen.home_location);
    }

    #[test]
    fn should_not_move_when_home_and_dead() {
        let grid = define_geography(50);
        let mut citizen = mother::dead_citizen();
        citizen.current_area = citizen.home_location;
        let new_destination = citizen.new_destination(RoutineTask::Dead, &grid);
        assert!(new_destination.is_none());
    }
}
