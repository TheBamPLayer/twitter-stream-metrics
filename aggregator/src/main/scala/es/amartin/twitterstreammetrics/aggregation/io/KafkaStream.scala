package es.amartin.twitterstreammetrics.aggregation.io

import java.util.Properties

import es.amartin.twitterstreammetrics.aggregation.config.Configuration
import org.apache.kafka.clients.consumer.ConsumerConfig
import org.apache.kafka.common.serialization.Serdes
import org.apache.kafka.streams.kstream.{KStream, KStreamBuilder}
import org.apache.kafka.streams.{KafkaStreams, StreamsConfig}

class KafkaStream(implicit val settings: Configuration) {
  private val stringSerde = Serdes.String()
  private val byteArraySerde = Serdes.Integer()

  private val properties = new Properties
  properties.put(ConsumerConfig.AUTO_OFFSET_RESET_CONFIG, settings.kafkaOffsetResetConfig)
  properties.put(StreamsConfig.APPLICATION_ID_CONFIG, settings.kafkaApplicationId)
  properties.put(StreamsConfig.BOOTSTRAP_SERVERS_CONFIG, settings.kafkaBrokers)
  properties.put(StreamsConfig.STATE_DIR_CONFIG, settings.kafkaStateDir)
  properties.put(StreamsConfig.NUM_STREAM_THREADS_CONFIG, settings.kafkaThreads)

  private val builder = new KStreamBuilder()

  private val streaming: KStream[Integer, String] = builder
    .stream(byteArraySerde, stringSerde, settings.kafkaInputTopic)

  def stream: KafkaStreams = new KafkaStreams(builder, properties)

  def inputStream: KStream[Integer, String] = streaming
}
