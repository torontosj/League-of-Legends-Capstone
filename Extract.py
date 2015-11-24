from datetime import datetime

from cassiopeia.type.api.store import SQLAlchemyDB
from cassiopeia import riotapi
from cassiopeia.type.api.exception import APIError
from cassiopeia.type.core.common import LoadPolicy

def main():
  riotapi.set_region("kr")
  riotapi.print_calls(True)
  riotapi.set_load_policy(LoadPolicy.lazy)

  riotapi.set_api_key("d045c326-dc9f-4de4-a463-793944aa6984")

  db = SQLAlchemyDB("mysql+mysqlconnector", "localhost", "kr_challenger_522", "root", "0123")
  riotapi.set_data_store(db)

  challenger = [entry.summoner for entry in list(riotapi.get_challenger()[1:10])]

  gather_start = datetime(2015, 11, 12) # 1 day after patch 5.19
  for summoner in challenger:
    for match in summoner.match_list(begin_time=gather_start):
      get_match(match)

  db.close()

def get_match(reference):
  try:
    riotapi.get_match(reference)
  except APIError as e:
    # Try Again Once
    if(e.error_code in [500]):
      try:
        riotapi.get_match(reference)
      except APIError as e2:
        if(e2.error_code in [500, 400, 404]):
          pass
        else:
          raise e2

    # Skip
    elif(e.error_code in [400, 404]):
      pass

    # Fatal
    else:
      raise e

if __name__ == "__main__":
    main()