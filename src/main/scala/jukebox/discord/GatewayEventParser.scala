package jukebox
package discord

import Json4sUtils._
import Json.formats
import GatewayEvents._

private[discord] object GatewayEventParser {
  def parse(event: DynJValueSelector): Option[GatewayEvent] = {
    val res = event.t.extract[String] match {
      case "READY" => Json.extract[Ready](event.d.jv)
      case "RESUMED" => Resumed
      case "CHANNEL_CREATE" => ChannelCreate(Json.extract[Channel](event.d.jv))
      case "CHANNEL_UPDATE" => ChannelUpdate(Json.extract[Channel](event.d.jv))
      case "CHANNEL_DELETE" => ChannelDelete(Json.extract[Channel](event.d.jv))
      case "GUILD_CREATE" => GuildCreate(Json.extract[GatewayEvents.Guild](event.d.jv))
      case "GUILD_UPDATE" => GuildUpdate(Json.extract[GatewayEvents.Guild](event.d.jv))
      case "GUILD_DELETE" => GuildDelete(Json.extract[UnavailableGuild](event.d.jv))
      case "GUILD_BAN_ADD" => GuildBanAdd(event.d.guildId.extract, Json.extract[User](event.d.jv))
      case "GUILD_BAN_REMOVE" => GuildBanRemove(event.d.guildId.extract, Json.extract[User](event.d.jv))
      case "GUILD_EMOJIS_UPDATE" => Json.extract[GuildEmojisUpdate](event.d.jv)
      case "GUILD_INTEGRATION_UPDATE" => Json.extract[GuildIntegrationUpdate](event.d.jv)
      case "GUILD_MEMBER_ADD" => GuildMemberAdd(event.d.guildId.extract, Json.extract[User](event.d.jv))
      case "GUILD_MEMBER_REMOVE" => Json.extract[GuildMemberRemove](event.d.jv)
      case "GUILD_MEMBER_UPDATE" => Json.extract[GuildMemberUpdate](event.d.jv)
      case "GUILD_MEMBER_CHUNK" => Json.extract[GuildMemberChunk](event.d.jv)
      case "GUILD_ROLE_CREATE" => Json.extract[GuildRoleCreate](event.d.jv)
      case "GUILD_ROLE_UPDATE" => Json.extract[GuildRoleUpdate](event.d.jv)
      case "GUILD_ROLE_DELETE" => Json.extract[GuildRoleDelete](event.d.jv)
      case "MESSAGE_CREATE" => MessageCreate(Json.extract[Message](event.d.jv))
      case "MESSAGE_UPDATE" => GatewayEvents.MessageUpdate(Json.extract[MessageUpdate](event.d.jv))
      case "MESSAGE_DELETE" => Json.extract[MessageDelete](event.d.jv)
      case "MESSAGE_DELETE_BULK" => Json.extract[MessageDeleteBulk](event.d.jv)
      case "PRESENCE_UPDATE" => Json.extract[PresenceUpdate](event.d.jv)
      case "TYPING_START" => Json.extract[TypingStart](event.d.jv)
      case "USER_UPDATE" => UserUpdate(Json.extract[User](event.d.jv))
      case "VOICE_STATE_UPDATE" => VoiceStateUpdate(event.d.user_id.extract, event.d.session_id.extract, Json.extract[VoiceState](event.d.jv))
      case "VOICE_SERVER_UPDATE" => Json.extract[VoiceServerUpdate](event.d.jv)
      case _ => null
    }
    Option(res)
  }
}
