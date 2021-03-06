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

use rdkafka::consumer::{StreamConsumer, Consumer};
use rdkafka::{ClientConfig, Message};

use rdkafka::error::{KafkaResult};
use rdkafka::message::BorrowedMessage;
use crate::environment;

const TICKS_TOPIC: &str = "ticks";

pub fn start(engine_id: &str) -> StreamConsumer {
    let kafka_url = environment::kafka_url();
    let consumer: StreamConsumer = ClientConfig::new()
        .set("bootstrap.servers", kafka_url.as_str())
        .set("group.id", engine_id)
        .set("auto.offset.reset", "earliest")
        .create()
        .expect("Consumer creation failed");

    consumer.subscribe(&[TICKS_TOPIC])
        .expect("Couldn't subscribe to specified topics");

    consumer
}

pub fn read(msg: Option<KafkaResult<BorrowedMessage>>) -> Option<i32> {
    match msg {
        None => {
            debug!("End of tick stream");
            None
        },
        Some(m) => {
            let borrowed_message = m.unwrap();
            let parsed_message = borrowed_message.payload_view::<str>().unwrap().unwrap();
            let tick: i32 = parsed_message.parse().expect("Could not parse tick message into an i32");
            Some(tick)
        },
    }
}
