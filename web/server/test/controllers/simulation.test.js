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

const app = require('../../app');
const supertest = require('supertest');
const request = supertest(app);

jest.mock('../../services/kafka');
jest.mock('../../db/models/Simulation');
jest.mock("../../services/kafka");

const KafkaServices = require("../../services/kafka");

const {Simulation} = require('../../db/models/Simulation');

describe('simulation controller', () => {

    const postData = {
        "number_of_agents": 10000,
        "public_transport_percentage": 0.2,
        "working_percentage": 0.7,
        "grid_size": 250,
        "simulation_hrs": 10000,
        "vaccinate_at": 5000,
        "vaccinate_percentage": 0.2,
        "lockdown_at_number_of_infections": 100,
        "essential_workers_population": 0.1,
        "hospital_spread_rate_threshold": 100,
        "disease_name": "small_pox",
        "regular_transmission_start_day": 10,
        "high_transmission_start_day": 16,
        "last_day": 22,
        "regular_transmission_rate": 0.05,
        "high_transmission_rate": 0.5,
        "death_rate": 0.2,
        "enable_citizen_state_messages": false
    };

    let kafkaService;

    afterAll(async () => {
        await app.close()
    });

    afterEach(() => {
        jest.clearAllMocks()
    });


    it('should insert simulation in db', async () => {
        const mockSave = jest.fn().mockReturnValueOnce(Promise.resolve());
        Simulation.mockReturnValueOnce({save: mockSave});

        await request
            .post('/simulation/init')
            .send({ ...postData });

        const simulationDocument = Simulation.mock.calls[0][0];
        expect(Simulation).toHaveBeenCalledTimes(1);
        expect(simulationDocument.simulation_id).toBeTruthy();
        expect(simulationDocument.config).toMatchSnapshot();
        expect(simulationDocument.status).toEqual('in-queue');
    });

    it('should update simulation has failed when sending message on kafka has failed', async () => {
        const mockSave = jest.fn().mockReturnValueOnce(Promise.resolve());
        Simulation.mockReturnValueOnce({save: mockSave});
        let mockFailingSend = jest.fn().mockRejectedValue(new Error("because we want to"));
        KafkaServices.KafkaProducerService.mockReturnValueOnce({send: mockFailingSend});
        const mockExec = jest.fn();
        Simulation.updateOne.mockReturnValueOnce({exec: mockExec});

        await request
            .post('/simulation/init')
            .send({ ...postData });

        const simulationDocument = Simulation.mock.calls[0][0];
        expect(Simulation).toHaveBeenCalledTimes(1);
        expect(simulationDocument.simulation_id).toBeTruthy();
        expect(simulationDocument.config).toMatchSnapshot();
        expect(simulationDocument.status).toEqual('in-queue');
        expect(Simulation.updateOne).toHaveBeenCalledTimes(1);
        expect(Simulation.updateOne.mock.calls[0][1]).toEqual({status: "failed"});
        expect(Simulation.updateOne.mock.calls[0][0]).toHaveProperty('simulation_id')
        expect(mockExec).toHaveBeenCalledTimes(1);
        expect(mockExec.mock.calls[0]).toEqual([]);
    });

    it('should write simulation start request on kafka topic after simulation db insert', async done => {
        kafkaService = require('../../services/kafka');
        Simulation.mockReturnValueOnce({save: jest.fn().mockReturnValueOnce(Promise.resolve())});

        const response = await request
            .post('/simulation/init')
            .send({ ...postData });

        const kafkaPayload = {
            enable_citizen_state_messages: false,
            population:
            {
                Auto:
                {
                    number_of_agents: 10000,
                    public_transport_percentage: 0.2,
                    working_percentage: 0.7
                }
            },
            disease:
            {
                regular_transmission_start_day: 10,
                high_transmission_start_day: 16,
                last_day: 22,
                regular_transmission_rate: 0.05,
                high_transmission_rate: 0.5,
                death_rate: 0.2
            },
            grid_size: 250,
            hours: 10000,
            interventions:
                [{
                    Vaccinate: {
                        at_hour: 5000,
                        percent: 0.2
                    },
                },
                {
                    Lockdown: {
                        at_number_of_infections: 100,
                        essential_workers_population: 0.1,
                        lock_down_period: 21
                    }
                },
                {
                    BuildNewHospital: {
                        spread_rate_threshold: 100
                    }
                }]
        }

        expect(kafkaService.KafkaProducerService).toHaveBeenCalled();

        const producerService = kafkaService.KafkaProducerService.mock.instances[0];

        expect(producerService.send.mock.calls[0][0]).toBe("simulation_requests");
        const payload = producerService.send.mock.calls[0][1];
        delete payload["sim_id"]; //it is a timestamp, cannot test
        expect(payload).toEqual(kafkaPayload);

        expect(response.status).toBe(200);
        done();
    });

    it('should not put vaccination intervention in kafka topic if params not available in /init POST request', async done => {
        kafkaService = require('../../services/kafka');
        Simulation.mockReturnValueOnce({save: jest.fn().mockReturnValueOnce(Promise.resolve())});


        const { vaccinate_at, vaccinate_percentage, ...postDataWithoutVaccinationIntervention } = { ...postData };

        const response = await request
            .post('/simulation/init')
            .send(postDataWithoutVaccinationIntervention);

        const kafkaPayload = {
            enable_citizen_state_messages: false,
            population:
            {
                Auto:
                {
                    number_of_agents: 10000,
                    public_transport_percentage: 0.2,
                    working_percentage: 0.7
                }
            },
            disease:
            {
                regular_transmission_start_day: 10,
                high_transmission_start_day: 16,
                last_day: 22,
                regular_transmission_rate: 0.05,
                high_transmission_rate: 0.5,
                death_rate: 0.2
            },
            grid_size: 250,
            hours: 10000,
            interventions:
                [{
                    Lockdown: {
                        at_number_of_infections: 100,
                        essential_workers_population: 0.1,
                        lock_down_period: 21
                    }
                },
                {
                    BuildNewHospital: {
                        spread_rate_threshold: 100
                    }
                }]
        };

        expect(kafkaService.KafkaProducerService).toHaveBeenCalledTimes(1);

        const producerService = kafkaService.KafkaProducerService.mock.instances[0];

        expect(producerService.send.mock.calls[0][0]).toBe("simulation_requests");
        const payload = producerService.send.mock.calls[0][1];
        delete payload["sim_id"]; //it is a timestamp, cannot test
        expect(payload).toEqual(kafkaPayload);

        expect(response.status).toBe(200);
        done();
    });
});
